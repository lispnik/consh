;;;; exec.lisp — the pipeline executor (SPEC §3 execution, §5 threads).
;;;;
;;;; run-pipeline turns a pipeline object into running processes + threads and
;;;; returns a pipeline-result whose final output is a lazy object-seq.  The
;;;; thread census follows SPEC §5: one reaper (already global), one pump thread
;;;; per parse/unparse boundary, one stderr drainer per external, one worker per
;;;; lisp group.  fd hygiene is centralized here: all pipes are built before any
;;;; spawn, every pipe end is close-on-exec, and the parent closes its child-side
;;;; copies immediately after spawning (missing this = "grep never sees EOF").

(in-package #:consh)

;;; ---------------------------------------------------------------------------
;;; Bookkeeping for a running pipeline
;;; ---------------------------------------------------------------------------

(defstruct ext-record
  "One running external stage."
  index stage invocation process
  (stderr-stream nil)      ; parent-side stream draining the child's stderr
  (stderr-text nil))       ; drained text, filled once the drainer finishes

(defstruct run-state
  (pgid nil)               ; shared process-group id of all externals
  (externals '())          ; list of ext-record, in pipeline order
  (pump-threads '())       ; parse/unparse boundary pumps
  (drainer-threads '())    ; stderr drainers
  (worker-threads '())     ; lisp-group workers
  (streams '())            ; parent-side streams to close on teardown
  (seqs '())               ; object-seqs to cancel on teardown
  (open-fds '())           ; raw pipe fds not yet handed off (closed on teardown)
  (torn-down nil))

(defclass pipeline-result ()
  ((pipeline   :initarg :pipeline   :reader pipeline-result-pipeline)
   (seq        :initarg :seq        :reader pipeline-result-seq)
   (state      :initarg :state      :reader pipeline-result-state)
   (on-failure :initarg :on-failure :reader pipeline-result-on-failure)
   (start-time :initarg :start-time :reader pipeline-result-start-time)
   (elapsed    :initform nil :accessor pipeline-result-elapsed))
  (:documentation
   "The handle for a running/completed pipeline: its final object-seq, the
external process records, and per-stage status.  History can hold these so old
outputs are recoverable without re-running (SPEC §3)."))

(defmethod print-object ((r pipeline-result) stream)
  (print-unreadable-object (r stream :type t)
    (format stream "~D external~:P~@[ ~Dms~]"
            (length (run-state-externals (pipeline-result-state r)))
            (pipeline-result-elapsed r))))

(defun pipeline-result-processes (result)
  "The external process objects, in pipeline order."
  (mapcar #'ext-record-process (run-state-externals (pipeline-result-state result))))

(defun pipeline-result-pump-count (result)
  "How many parse/unparse pump threads the run created.  For a pure
external→external pipeline this is 1 (only the tail parse) — proof that no bytes
were routed through Lisp between the two externals."
  (length (run-state-pump-threads (pipeline-result-state result))))

;;; ---------------------------------------------------------------------------
;;; Failure conditions
;;; ---------------------------------------------------------------------------

(define-condition pipeline-failed (command-failed)
  ((stage       :initarg :stage       :initform nil :reader pipeline-failed-stage)
   (stage-index :initarg :stage-index :initform nil :reader pipeline-failed-stage-index))
  (:report (lambda (c s)
             (format s "pipeline stage ~@[~D ~]~@[(~A) ~]failed with status ~A"
                     (pipeline-failed-stage-index c)
                     (let ((cmd (command-failed-command c)))
                       (and cmd (invocation-program cmd)))
                     (command-failed-status c))))
  (:documentation
   "A stage of a pipeline exited in failure under :on-failure :signal.  Names
the offending stage and carries its parsed stderr (SPEC §3)."))

;;; ---------------------------------------------------------------------------
;;; fd plumbing helpers
;;; ---------------------------------------------------------------------------

(defun %fd-input-stream (fd)
  (sb-sys:make-fd-stream fd :input t :element-type 'character :buffering :full))

(defun %fd-output-stream (fd)
  (sb-sys:make-fd-stream fd :output t :element-type 'character :buffering :full))

(defparameter *devnull-stdin*
  (list :open 0 "/dev/null" sb-posix:o-rdonly 0)
  "A spawn file action pointing the child's stdin at /dev/null.")

;;; Every pipe fd is registered in the run-state the moment it is created, so a
;;; failure part-way through building a group still tears down cleanly (no leaked
;;; fds, no orphaned processes).  fds are then either closed (child-side copies)
;;; or adopted by a parent-side stream — in both cases removed from OPEN-FDS so
;;; teardown never double-closes a since-reused fd number.

(defun %pipe (state)
  "Create a pipe and register both ends in STATE's open-fds.  Returns (r w)."
  (multiple-value-bind (r w) (make-pipe)
    (push r (run-state-open-fds state))
    (push w (run-state-open-fds state))
    (values r w)))

(defun %close-fd (state fd)
  "Close FD (a child-side copy) and drop it from open-fds."
  (setf (run-state-open-fds state) (delete fd (run-state-open-fds state)))
  (ignore-errors (c-close fd)))

(defun %adopt-stream (state fd direction)
  "Wrap FD in a parent-side stream tracked for teardown; drop it from open-fds
so it is closed once, via the stream."
  (setf (run-state-open-fds state) (delete fd (run-state-open-fds state)))
  (let ((s (ecase direction
             (:input (%fd-input-stream fd))
             (:output (%fd-output-stream fd)))))
    (push s (run-state-streams state))
    s))

;;; ---------------------------------------------------------------------------
;;; Running an external group (a maximal run of external stages)
;;; ---------------------------------------------------------------------------

(defun %run-external-group (stages input-seq state base-index on-parse-error)
  "Spawn STAGES as one process group joined by kernel pipes.  INPUT-SEQ (an
object-seq or NIL) feeds the first stage's stdin via an unparse pump; the last
stage's stdout is parsed into the returned object-seq.  Updates STATE."
  (let* ((n (length stages))
         (invs (mapcar #'stage-invocation stages))
         (internal (loop repeat (1- n) collect (multiple-value-list (%pipe state))))
         (stderr-pipes (loop repeat n collect (multiple-value-list (%pipe state))))
         ;; boundary pipes
         (in-pipe  (when input-seq (multiple-value-list (%pipe state))))
         (out-pipe (multiple-value-list (%pipe state)))
         (pgid 0)
         (records '()))
    (flet ((stdin-of (i)
             (cond ((> i 0) (first (nth (1- i) internal)))     ; read end of prev pipe
                   (in-pipe (first in-pipe))                   ; unparse read end
                   (t nil)))                                   ; -> /dev/null
           (stdout-of (i)
             (if (< i (1- n))
                 (second (nth i internal))                     ; write end to next
                 (second out-pipe))))                          ; run's stdout
      ;; --- spawn every stage, joined into one process group.  Register each
      ;;     process (and the pgid) immediately, so a spawn failure part-way
      ;;     through still lets %teardown kill what already started.
      (loop for i from 0 below n
            for inv in invs
            for stage in stages
            for stderr-w = (second (nth i stderr-pipes))
            for actions = (let ((a (list (list :dup2 (stdout-of i) 1)
                                         (list :dup2 stderr-w 2))))
                            (let ((in (stdin-of i)))
                              (if in
                                  (cons (list :dup2 in 0) a)
                                  (cons *devnull-stdin* a))))
            do (let ((proc (launch (invocation-program inv)
                                   (invocation-arguments inv)
                                   :pgid pgid :file-actions actions)))
                 (when (zerop pgid)                             ; first = group leader
                   (setf pgid (process-pid proc))
                   (unless (run-state-pgid state)
                     (setf (run-state-pgid state) pgid)))
                 (let ((rec (make-ext-record :index (+ base-index i) :stage stage
                                             :invocation inv :process proc)))
                   (push rec records)
                   (setf (run-state-externals state)
                         (append (run-state-externals state) (list rec))))))
      (setf records (nreverse records))
      ;; --- close every child-side fd in the parent (fd hygiene) ---
      (dolist (p internal) (%close-fd state (first p)) (%close-fd state (second p)))
      (when in-pipe (%close-fd state (first in-pipe)))     ; child's stdin read end
      (%close-fd state (second out-pipe))                  ; child's stdout write end
      (dolist (p stderr-pipes) (%close-fd state (second p))) ; child stderr write ends
      ;; --- stderr drainers (one per external, drained concurrently) ---
      (loop for rec in records
            for p in stderr-pipes
            do (let ((stream (%adopt-stream state (first p) :input))
                     (r rec))
                 (setf (ext-record-stderr-stream r) stream)
                 (push (spawn-stage-thread
                        (lambda ()
                          (setf (ext-record-stderr-text r) (slurp-stream stream)))
                        :name "consh-stderr")
                       (run-state-drainer-threads state))))
      ;; --- unparse pump: feed the first stage's stdin from INPUT-SEQ ---
      (when in-pipe
        (let ((ostream (%adopt-stream state (second in-pipe) :output))
              (inv0 (first invs)))
          (push (spawn-stage-thread
                 (lambda ()
                   (unwind-protect
                        (handler-case
                            (do-object-seq (obj input-seq)
                              (unparse-input inv0 (list obj) ostream))
                          (error () nil))       ; broken pipe: downstream gone
                     (ignore-errors (close ostream))))  ; EOF to the child
                 :name "consh-unparse")
                (run-state-pump-threads state))))
      ;; --- parse pump: the run's stdout becomes the output object-seq ---
      (let* ((istream (%adopt-stream state (first out-pipe) :input))
             (tail-inv (car (last invs)))
             (seq (parse-output tail-inv istream :on-parse-error on-parse-error)))
        (push (object-seq-thread seq) (run-state-pump-threads state))
        (push seq (run-state-seqs state))
        seq))))

;;; ---------------------------------------------------------------------------
;;; Running a lisp group (fused transducers in one worker)
;;; ---------------------------------------------------------------------------

(defun %run-lisp-group (stages input-seq state)
  "Run fused lisp STAGES as one worker: read INPUT-SEQ, apply the composed
transducer, emit into a fresh channel.  Returns the output object-seq."
  (let* ((xform (compose-xforms stages))
         (out (make-channel))
         (thread
           (spawn-stage-thread
            (lambda ()
              (unwind-protect
                   (call-with-worker-guard "lisp"
                    (lambda ()
                      (handler-case
                          (let ((step (funcall xform (lambda (o) (channel-put out o)))))
                            (when input-seq
                              (do-object-seq (x input-seq)
                                ;; per-object restart so a parked lisp worker can
                                ;; be told to skip a bad object (SPEC §5)
                                (restart-case (funcall step x)
                                  (skip-object ()
                                    :report "Skip this object and continue.")))))
                        ;; downstream cancelled: stop and cascade upstream
                        (channel-closed () (when input-seq (seq-close input-seq))))))
                (close-channel out)))
            :name "consh-lisp"))
         (seq (%make-object-seq out thread)))
    (push thread (run-state-worker-threads state))
    (push seq (run-state-seqs state))
    seq))

;;; ---------------------------------------------------------------------------
;;; run-pipeline
;;; ---------------------------------------------------------------------------

(defun seq-of-list (list)
  "An object-seq that emits the elements of LIST."
  (emitting (emit) (dolist (x list) (funcall emit x))))

(defun run-pipeline (pipeline &key (on-failure :collect) (on-parse-error :use-raw-lines) input)
  "Execute PIPELINE.  Returns a pipeline-result whose PIPELINE-RESULT-SEQ is the
final lazy object sequence.  :INPUT (a list or object-seq) feeds the head stage.
:ON-FAILURE is :collect (record statuses) or :signal (raise on the first failed
external when the result is collected).  :ON-PARSE-ERROR is the recovery policy
for the parse pumps (default :use-raw-lines; a job may pass :error so parse
failures park the worker)."
  (ensure-reaper)
  (let* ((state (make-run-state))
         (groups (pipeline-groups pipeline))
         (seq (etypecase input
                (null nil)
                (object-seq input)
                (list (and input (seq-of-list input)))))
         (base 0))
    (handler-case
        (dolist (g groups)
          (let ((stages (cdr g)))
            (setf seq (ecase (car g)
                        (:external (%run-external-group stages seq state base on-parse-error))
                        (:lisp (%run-lisp-group stages seq state))))
            (incf base (length stages))))
      (error (e)
        (%teardown state)
        (error e)))
    (make-instance 'pipeline-result
                   :pipeline pipeline :seq seq :state state
                   :on-failure on-failure :start-time (get-internal-real-time))))

;;; ---------------------------------------------------------------------------
;;; Teardown & cancellation
;;; ---------------------------------------------------------------------------

(defun %teardown (state)
  "Cancel and reclaim everything a run owns.  Idempotent.  killpg first so any
process blocked in read/write dies and its stdout EOFs (unblocking pump
read-lines); then cancel channels, join threads, close streams."
  (unless (run-state-torn-down state)
    (setf (run-state-torn-down state) t)
    (let ((pgid (run-state-pgid state)))
      (when pgid
        (ignore-errors (c-killpg pgid +sigkill+))))
    ;; wake anything blocked on a channel put/take
    (dolist (seq (run-state-seqs state))
      (ignore-errors (close-for-reading (object-seq-channel seq))))
    ;; join every worker/pump/drainer
    (dolist (th (append (run-state-worker-threads state)
                        (run-state-pump-threads state)
                        (run-state-drainer-threads state)))
      (when (and th (sb-thread:thread-alive-p th))
        (ignore-errors (sb-thread:join-thread th :timeout 5))))
    ;; close parent-side streams
    (dolist (s (run-state-streams state))
      (ignore-errors (close s)))
    ;; close any raw fds that were never handed off (e.g. a spawn failed
    ;; part-way through building the group)
    (dolist (fd (run-state-open-fds state))
      (ignore-errors (c-close fd)))
    (setf (run-state-open-fds state) '())
    ;; reap the processes
    (dolist (rec (run-state-externals state))
      (ignore-errors (wait-process (ext-record-process rec) :timeout 2)))
    t))

(defmethod runnable-seq ((x pipeline-result))
  "For take/one-shot consumers: the result seq, with pipeline teardown."
  (values (pipeline-result-seq x)
          (lambda () (%teardown (pipeline-result-state x)))))

(defmethod runnable-seq ((x pipeline))
  (runnable-seq (run-pipeline x)))

;;; ---------------------------------------------------------------------------
;;; Consuming a pipeline to completion, with :on-failure handling
;;; ---------------------------------------------------------------------------

(defun ensure-result (x)
  (etypecase x
    (pipeline-result x)
    (pipeline (run-pipeline x))))

(defun %exit-status (process)
  "An integer status for PROCESS: exit code, or 128+signal if killed."
  (case (process-status process)
    (:exited (process-exit-code process))
    (:signaled (+ 128 (process-term-signal process)))
    (t 0)))

(defun %first-failure (result)
  "The first external stage (in order) whose exit is a genuine failure — using
each wrapper's parse-error-output so e.g. grep's exit 1 is benign.  Returns a
pipeline-failed condition object, or NIL."
  (dolist (rec (run-state-externals (pipeline-result-state result)))
    (let* ((proc (ext-record-process rec))
           (status (%exit-status proc))
           (stderr (or (ext-record-stderr-text rec) ""))
           (cond (parse-error-output (ext-record-invocation rec)
                                     (make-string-input-stream stderr) status)))
      (when cond
        (return
          (make-condition 'pipeline-failed
                          :stage (ext-record-stage rec)
                          :stage-index (ext-record-index rec)
                          :command (ext-record-invocation rec)
                          :status status :stderr stderr))))))

(defun %finish (result)
  (setf (pipeline-result-elapsed result)
        (round (* 1000 (- (get-internal-real-time) (pipeline-result-start-time result)))
               internal-time-units-per-second)))

(defun pipeline-collect (x)
  "Run X (a pipeline or pipeline-result) to completion and return the final
objects as a list.  Under :on-failure :signal, raises PIPELINE-FAILED for the
first failed stage; the RESTART-STAGE restart reruns the (optionally corrected)
pipeline (SPEC §3)."
  (let ((result (ensure-result x)))
    (unwind-protect
         (restart-case
             (let ((objs (seq-collect (pipeline-result-seq result))))
               (dolist (rec (run-state-externals (pipeline-result-state result)))
                 (wait-process (ext-record-process rec) :timeout 10))
               (%finish result)
               (when (eq (pipeline-result-on-failure result) :signal)
                 (let ((failure (%first-failure result)))
                   (when failure (error failure))))
               objs)
           (restart-stage (&optional (replacement (pipeline-result-pipeline result)))
             :report "Rerun the (optionally corrected) pipeline from the start."
             (pipeline-collect replacement)))
      (%teardown (pipeline-result-state result)))))
