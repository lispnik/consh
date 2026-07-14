;;;; parse.lisp — the per-command parser protocol (SPEC.md §2).
;;;;
;;;; The boundary between bytes and objects.  parse-output turns an external
;;;; command's stdout stream into a LAZY, channel-backed sequence of objects;
;;;; unparse-input serializes objects back into a command's stdin; and
;;;; parse-error-output translates a nonzero exit + stderr into a typed
;;;; condition.  Everything dispatches on invocation objects, so a new wrapper
;;;; is just a set of defmethods.

(in-package #:consh)

;;; ---------------------------------------------------------------------------
;;; Shell-special propagation to worker threads (SPEC.md §5)
;;; ---------------------------------------------------------------------------

(defvar *worker-error-hook* nil
  "When bound (by a running job), a function (LABEL CONDITION) called from inside
a worker's handler-bind on an unhandled serious-condition — with the stack
intact, so all restarts are still live (SPEC.md §5 cross-thread conditions).  It
typically parks the worker and posts a job event.")

(defvar *shell-specials* '(*current-directory* *channel-stop-flag* *worker-error-hook*)
  "Dynamic variables snapshotted at pipeline start and rebound in every worker
thread.  Wrappers and stages that spawn threads must go through
SPAWN-STAGE-THREAD so these follow along (never a naked make-thread).")

(defun call-with-worker-guard (label thunk)
  "Run THUNK; if *WORKER-ERROR-HOOK* is set, install a handler-bind so an
unhandled serious-condition (other than normal CHANNEL-CLOSED cancellation) is
routed to the hook with the stack — and thus the live restarts — intact.  If the
hook declines (or an error slips past), the worker dies GRACEFULLY here rather
than letting the condition reach the thread top — which, with the debugger
disabled, would quit the whole image.  The caller's unwind-protect still closes
the worker's channel, so downstream sees EOF."
  (let ((hook *worker-error-hook*))
    ;; The handler-case wraps BOTH paths: with a hook, the handler-bind first
    ;; routes the live condition to the hook (stack/restarts intact); with no
    ;; hook (a bare foreground pipeline), we still catch here so the worker dies
    ;; gracefully — never letting the condition reach the thread top, which would
    ;; quit the whole image.  Either way the caller's unwind-protect closes the
    ;; channel so downstream sees EOF.
    (handler-case
        (if hook
            (handler-bind ((serious-condition
                             (lambda (c)
                               (unless (typep c 'channel-closed)
                                 (funcall hook label c)))))
              (funcall thunk))
            (funcall thunk))
      (serious-condition () nil))))

(defun spawn-stage-thread (function &key name)
  "Start a worker thread running FUNCTION with *SHELL-SPECIALS* rebound to the
values they hold now, so e.g. *current-directory* is consistent across the
whole pipeline regardless of which thread does the work."
  (let ((vars *shell-specials*)
        (vals (mapcar #'symbol-value *shell-specials*)))
    (sb-thread:make-thread
     (lambda () (progv vars vals (funcall function)))
     :name (or name "consh-stage"))))

;;; ---------------------------------------------------------------------------
;;; Lazy, channel-backed object sequences
;;; ---------------------------------------------------------------------------
;;;
;;; parse-output must return a LAZY sequence whose early termination can kill the
;;; producer (SPEC.md §2/§4).  We back it with a Phase-2 channel and a producer
;;; thread: the producer calls (emit obj) -> channel-put; a consumer takes until
;;; +channel-eof+.  seq-close performs downstream cancellation: close-for-reading
;;; wakes the producer's next put with CHANNEL-CLOSED and it stops.

(defstruct (object-seq (:constructor %make-object-seq (channel thread))
                       (:predicate object-seq-p))
  "A lazy sequence of objects produced by a background thread into a bounded
channel."
  (channel nil :read-only t)
  (thread  nil :read-only t))

(defun %parse-error-handler (policy)
  "Build a handler function implementing an :on-parse-error POLICY, one of
:error (decline -> propagate), :use-raw-lines (recover with the raw text), or a
function of the condition."
  (lambda (c)
    (cond ((eq policy :use-raw-lines) (invoke-restart 'use-raw-lines))
          ((functionp policy) (funcall policy c))
          (t nil))))                    ; :error and anything else: decline

(defun spawn-object-seq (producer &key (capacity 256) (stop-flag *channel-stop-flag*)
                                       (on-parse-error :error) (label "parse"))
  "Run PRODUCER — a function of one argument EMIT — in a stage thread, feeding a
bounded channel.  Returns an OBJECT-SEQ.  PRODUCER calls (funcall EMIT obj) for
each object; when it returns the channel is closed (EOF).  A PARSE-ERROR signaled
inside PRODUCER is handled per ON-PARSE-ERROR, in the producer thread (SPEC.md
§5: handler specs live inside the worker).  Under a running job, any other
unhandled serious-condition parks the worker via *WORKER-ERROR-HOOK* (LABEL
identifies the stage)."
  (let* ((channel (make-channel :capacity capacity :stop-flag stop-flag))
         (thread
           (spawn-stage-thread
            (lambda ()
              (unwind-protect
                   (call-with-worker-guard label
                    (lambda ()
                      (handler-case
                          (handler-bind ((parse-error (%parse-error-handler on-parse-error)))
                            (funcall producer
                                     (lambda (object) (channel-put channel object))))
                        ;; Downstream cancelled us: stop quietly.
                        (channel-closed () nil))))
                (close-channel channel)))
            :name "consh-parse")))
    (%make-object-seq channel thread)))

(defmacro emitting ((emit &key capacity stop-flag on-parse-error label) &body body)
  "Sugar for SPAWN-OBJECT-SEQ: BODY runs in a producer thread and yields objects
by calling (funcall EMIT obj).  Returns the lazy OBJECT-SEQ."
  `(spawn-object-seq (lambda (,emit) ,@body)
                     ,@(when capacity `(:capacity ,capacity))
                     ,@(when stop-flag `(:stop-flag ,stop-flag))
                     ,@(when on-parse-error `(:on-parse-error ,on-parse-error))
                     ,@(when label `(:label ,label))))

(defun seq-next (seq)
  "Take the next object from SEQ.  Returns (values object t), or (values nil nil)
at end of sequence."
  (let ((object (channel-take (object-seq-channel seq))))
    (if (eof-p object) (values nil nil) (values object t))))

(defun seq-take (n seq)
  "Take up to N objects from SEQ as a list, leaving SEQ open."
  (loop repeat n
        for object = (channel-take (object-seq-channel seq))
        until (eof-p object)
        collect object))

(defun seq-collect (seq)
  "Drain SEQ to the end, returning all objects as a list.  The producer runs to
completion."
  (loop for object = (channel-take (object-seq-channel seq))
        until (eof-p object)
        collect object))

(defun seq-close (seq)
  "Early termination: cancel SEQ's producer (downstream cancellation) and reclaim
its thread.  Returns SEQ."
  (close-for-reading (object-seq-channel seq))
  (let ((thread (object-seq-thread seq)))
    (when (and thread (sb-thread:thread-alive-p thread))
      (sb-thread:join-thread thread)))
  seq)

(defgeneric runnable-seq (x)
  (:documentation
   "Coerce X to (values object-seq teardown-thunk).  The default handles a bare
object-seq; pipeline.lisp/exec.lisp add methods so a pipeline can be run and torn
down through the same one-shot consumers (take, etc.).")
  (:method ((x object-seq)) (values x (lambda () (seq-close x)))))

(defun take (n x)
  "Take the first N objects of X (an object-seq, or anything RUNNABLE-SEQ
accepts such as a pipeline), then KILL the producer / tear down (SPEC.md §2
take).  Returns the list of objects."
  (multiple-value-bind (seq teardown) (runnable-seq x)
    (prog1 (seq-take n seq)
      (funcall teardown))))

(defmacro do-object-seq ((var seq &optional result) &body body)
  "Iterate VAR over every object of SEQ."
  (let ((s (gensym "SEQ")) (more (gensym "MORE")))
    `(let ((,s ,seq))
       (loop
         (multiple-value-bind (,var ,more) (seq-next ,s)
           (unless ,more (return ,result))
           ,@body)))))

;;; ---------------------------------------------------------------------------
;;; parse-error condition + record-level restarts (SPEC.md §2)
;;; ---------------------------------------------------------------------------

(define-condition parse-error (error)
  ((command :initarg :command :initform nil :reader parse-error-command)
   (raw     :initarg :raw     :initform nil :reader parse-error-raw)
   (cause   :initarg :cause   :initform nil :reader parse-error-cause))
  (:report (lambda (c s)
             (format s "cannot parse ~S~@[ (~A)~]"
                     (parse-error-raw c) (parse-error-cause c))))
  (:documentation
   "A wrapper could not parse one record of a command's output.  Handled with
the restarts USE-RAW-LINES, TRY-DIALECT, DEFINE-PARSER (SPEC.md §2)."))

(defun signal-parse-error (command raw &optional cause)
  "Signal a PARSE-ERROR for record RAW of COMMAND (CAUSE optional)."
  (error 'parse-error :command command :raw raw :cause cause))

(defun parse-record (command raw thunk)
  "Parse one record: call THUNK to produce the object for RAW, establishing the
recovery restarts around it.  If THUNK signals PARSE-ERROR, a handler may invoke:
  USE-RAW-LINES  -> yield RAW unchanged (a string),
  TRY-DIALECT    -> re-run THUNK (dialect scaffold),
  DEFINE-PARSER  -> parse RAW with a supplied function."
  ;; COMMAND is part of the record-parsing API (callers pass the invocation for
  ;; symmetry and future dialect-aware restarts) though this scaffold ignores it.
  (declare (ignorable command))
  (restart-case (funcall thunk)
    (use-raw-lines ()
      :report (lambda (s) (format s "Yield the raw text ~S as a string." raw))
      raw)
    (try-dialect (&optional dialect)
      :report "Retry parsing this record under a different command dialect."
      ;; switch the invocation's dialect (to DIALECT, or the next candidate) and
      ;; re-run the parse — the thunk reads (command-dialect command)
      (when command
        (setf (invocation-dialect command)
              (or dialect (next-dialect (invocation-dialect command)))))
      (funcall thunk))
    (define-parser (parser)
      :report "Parse this record with a supplied function of the raw text."
      (funcall parser raw))))

;;; ---------------------------------------------------------------------------
;;; External-command failure (SPEC.md §2; CLAUDE.md: subtype of command-failed)
;;; ---------------------------------------------------------------------------

(define-condition command-failed (error)
  ((command :initarg :command :initform nil :reader command-failed-command)
   (status  :initarg :status  :initform nil :reader command-failed-status)
   (stderr  :initarg :stderr  :initform nil :reader command-failed-stderr))
  (:report (lambda (c s)
             (format s "command ~@[~A ~]failed with status ~A~@[:~%~A~]"
                     (let ((cmd (command-failed-command c)))
                       (and cmd (invocation-program cmd)))
                     (command-failed-status c)
                     (let ((e (command-failed-stderr c)))
                       (and e (plusp (length e)) e)))))
  (:documentation
   "Base type for every external-command failure.  Wrappers translate specific
exit codes into subtypes carrying parsed detail (SPEC.md §2 rsync example)."))

(defun slurp-stream (stream)
  "Read all remaining characters of STREAM into a string.  NIL stream -> \"\"."
  (if (null stream)
      ""
      (with-output-to-string (out)
        (loop for line = (read-line stream nil nil)
              for first = t then nil
              while line
              do (unless first (terpri out))
                 (write-string line out)))))

(defun split-whitespace (string)
  "Split STRING on runs of spaces/tabs, dropping empty fields.  Used by wrappers
whose output is whitespace-columned (df, wc, ...)."
  (let ((fields '()) (start nil) (len (length string)))
    (dotimes (i len)
      (let ((ws (member (char string i) '(#\Space #\Tab))))
        (cond (ws (when start (push (subseq string start i) fields) (setf start nil)))
              ((null start) (setf start i)))))
    (when start (push (subseq string start) fields))
    (nreverse fields)))

(defun join-with-space (strings)
  "Join STRINGS with single spaces."
  (format nil "~{~A~^ ~}" strings))

;;; ---------------------------------------------------------------------------
;;; Protocol generics + default methods
;;; ---------------------------------------------------------------------------

(defgeneric parse-output (command stream &key &allow-other-keys)
  (:documentation
   "Return a lazy OBJECT-SEQ of objects parsed from STREAM (the command's
stdout).  The default method yields the stream's lines as strings.  Wrappers
override to yield structured objects.  :ON-PARSE-ERROR selects the recovery
policy applied inside the producer thread.")
  (:method ((command t) stream &key (on-parse-error :error) &allow-other-keys)
    (emitting (emit :on-parse-error on-parse-error)
      (loop for line = (read-line stream nil nil)
            while line
            do (funcall emit line)))))

(defgeneric parse-error-output (command stream status)
  (:documentation
   "Translate a command's exit STATUS and stderr STREAM into a condition object
\(returned, not signaled), or NIL if STATUS is not actually a failure for this
command.  The default treats any non-zero status as a generic COMMAND-FAILED.")
  (:method ((command t) stream status)
    (if (and (integerp status) (zerop status))
        nil
        (make-condition 'command-failed
                        :command command :status status
                        :stderr (slurp-stream stream)))))

(defgeneric unparse-input (command objects stream)
  (:documentation
   "Serialize OBJECTS onto STREAM as the command's stdin.  The default prints
each object with PRINC followed by a newline.")
  (:method ((command t) objects stream)
    (dolist (object objects)
      (princ object stream)
      (terpri stream))
    (finish-output stream)))

(defgeneric rewrite-invocation (command)
  (:documentation
   "The rewriting half of run-and-parse (SPEC.md §2): a wrapper may return a new
invocation requesting a machine-readable output format (e.g. add --json or
-print0) before it is run.  The default returns COMMAND unchanged.  (Actually
spawning the rewritten command is the Phase 4 executor's job.)")
  (:method ((command t)) command))
