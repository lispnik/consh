;;;; dialect.lisp — command-dialect probing (SPEC §2).
;;;;
;;;; The same command name behaves differently across GNU (Linux) and BSD
;;;; (macOS) — flags and output formats diverge (`stat -c` vs `stat -f`,
;;;; `sed -i` vs `sed -i ''`, ...).  A wrapper whose flags/parsing depend on the
;;;; implementation probes it once — running `cmd --version` and classifying the
;;;; result — caches the answer per command name, and branches on it.  The
;;;; probe is per-COMMAND, not per-OS: `bash` reports GNU even on macOS.

(in-package #:consh)

(defvar *dialect-cache* (make-hash-table :test 'equal)
  "Program name (string) -> probed dialect keyword, stable for the session.")
(defvar *dialect-cache-lock* (sb-thread:make-mutex :name "dialect-cache"))

(defun reset-dialect-cache ()
  "Forget all probed dialects (mostly for tests)."
  (sb-thread:with-mutex (*dialect-cache-lock*) (clrhash *dialect-cache*)))

(defun %classify-dialect-text (text exit-code)
  "Classify the combined --version output TEXT (and EXIT-CODE) as :gnu, :bsd, or
:unknown.  GNU tools announce GNU/coreutils; BSD tools reject --version with a
usage/\"illegal option\" message (often still exiting 0 on macOS)."
  (cond
    ((or (search "GNU" text) (search "coreutils" text)) :gnu)
    ((or (search "illegal option" text) (search "usage:" text) (search "BSD" text)
         (and (integerp exit-code) (/= exit-code 0)))
     :bsd)
    (t :unknown)))

(defun %close-fd-safely (fd) (ignore-errors (c-close fd)))

(defun %capture (program args &key (timeout 5))
  "Run PROGRAM with ARGS, returning (values stdout stderr exit-code).  EXIT-CODE
is NIL if the program could not be spawned.  Both streams are drained in their
own threads (so neither a child blocking on a full pipe nor a hung child
blocking our read can wedge us), and a child that outlives TIMEOUT is killed."
  (let (o-r o-w e-r e-w os es)
    (unwind-protect
         (handler-case
             (progn
               (setf (values o-r o-w) (make-pipe)
                     (values e-r e-w) (make-pipe))
               (let ((proc (launch program args
                                   :file-actions (list (list :dup2 o-w 1)
                                                       (list :dup2 e-w 2)))))
                 (%close-fd-safely o-w) (setf o-w nil)
                 (%close-fd-safely e-w) (setf e-w nil)
                 ;; hand each read end to a stream one at a time, so a failure
                 ;; building the second can't double-close the first's fd
                 (setf os (sb-sys:make-fd-stream o-r :input t :element-type 'character))
                 (setf o-r nil)
                 (setf es (sb-sys:make-fd-stream e-r :input t :element-type 'character))
                 (setf e-r nil)
                 (let* ((out "") (err "")
                        (t-out (spawn-stage-thread (lambda () (setf out (slurp-stream os)))
                                                   :name "consh-probe-out"))
                        (t-err (spawn-stage-thread (lambda () (setf err (slurp-stream es)))
                                                   :name "consh-probe-err")))
                   ;; bound the whole thing: kill a child that outlives TIMEOUT,
                   ;; which EOFs both pipes and lets the drainers finish/join
                   (unless (wait-process proc :timeout timeout)
                     (ignore-errors (kill-process proc))
                     (ignore-errors (wait-process proc :timeout timeout)))
                   (ignore-errors (sb-thread:join-thread t-out :timeout timeout))
                   (ignore-errors (sb-thread:join-thread t-err :timeout timeout))
                   (values out err (and (eq (process-status proc) :exited)
                                        (process-exit-code proc))))))
           (spawn-error () (values "" "" nil)))
      ;; the drainers are joined above, so closing the streams here is safe;
      ;; also close any raw fd not yet owned by a stream (early-error paths)
      (when os (ignore-errors (close os)))
      (when es (ignore-errors (close es)))
      (dolist (fd (list o-r o-w e-r e-w))
        (when fd (%close-fd-safely fd))))))

(defun probe-dialect (program)
  "Probe (and memoize) PROGRAM's dialect by running `program --version`.
Returns :gnu, :bsd, or :unknown."
  (multiple-value-bind (cached present)
      (sb-thread:with-mutex (*dialect-cache-lock*) (gethash program *dialect-cache*))
    (if present
        cached
        ;; probe outside the lock (it spawns a process); a racing double-probe is
        ;; harmless — same program yields the same answer
        (let ((result (multiple-value-bind (out err code) (%capture program '("--version"))
                        (%classify-dialect-text (concatenate 'string out " " err) code))))
          (sb-thread:with-mutex (*dialect-cache-lock*)
            (setf (gethash program *dialect-cache*) result))))))

(defun ensure-dialect (invocation program)
  "INVOCATION's dialect, probing PROGRAM and caching it on the slot when the slot
is still :unknown.  Wrappers call this from their COMMAND-DIALECT method."
  (if (eq (invocation-dialect invocation) :unknown)
      (setf (invocation-dialect invocation) (probe-dialect program))
      (invocation-dialect invocation)))
