;;;; jobs.lisp — jobs, fg/bg, C-z, job events, and debug-job (SPEC §5, §6).
;;;;
;;;; A job wraps a running pipeline (its subprocess set + one pgid + Lisp thread
;;;; set + channels) and extends job control over the threads:
;;;;
;;;;   * bg — a sink thread drains the pipeline's output into a buffer so the job
;;;;     progresses without the REPL; fg waits for it and returns the buffer.
;;;;   * C-z (stop-job) — SIGTSTP the pgid AND pause the shared stop-flag, parking
;;;;     the Lisp workers at their next channel op.  continue-job / fg / bg resume.
;;;;   * An unhandled condition in a worker parks that worker (not crash): it posts
;;;;     a job event and waits, with the stack — hence all restarts — intact.
;;;;     debug-job attaches and invokes a restart in the worker's own context,
;;;;     resuming the frozen line end to end.

(in-package #:consh)

;;; ---------------------------------------------------------------------------
;;; Bounded ring buffer (scrollback for background job output)
;;; ---------------------------------------------------------------------------
;;;
;;; A background job's output must not accumulate without limit — `yes &` would
;;; otherwise OOM (SPEC §6: "tees to ring buffer").  The sink keeps the most
;;; recent CAPACITY objects, dropping the oldest, and remembers how many it
;;; dropped.

(defvar *default-job-buffer-capacity* 65536
  "Default number of objects a background job's output ring retains.")

(defstruct (ring-buffer (:constructor %make-ring-buffer (capacity data)))
  (capacity 0 :type fixnum :read-only t)
  (data nil :type simple-vector)
  (head 0 :type fixnum)                 ; index of the oldest live element
  (count 0 :type fixnum)                ; number of live elements
  (dropped 0 :type fixnum))             ; total elements dropped on overflow

(defun make-ring-buffer (capacity)
  (check-type capacity (integer 1))
  (%make-ring-buffer capacity (make-array capacity :initial-element nil)))

(defun ring-push (ring item)
  "Append ITEM.  When full, overwrite the oldest element and count it dropped."
  (let ((cap (ring-buffer-capacity ring)))
    (if (< (ring-buffer-count ring) cap)
        (setf (svref (ring-buffer-data ring)
                     (mod (+ (ring-buffer-head ring) (ring-buffer-count ring)) cap))
              item
              (ring-buffer-count ring) (1+ (ring-buffer-count ring)))
        (setf (svref (ring-buffer-data ring) (ring-buffer-head ring)) item
              (ring-buffer-head ring) (mod (1+ (ring-buffer-head ring)) cap)
              (ring-buffer-dropped ring) (1+ (ring-buffer-dropped ring)))))
  item)

(defun ring-list (ring)
  "The retained elements, oldest first."
  (let ((cap (ring-buffer-capacity ring)))
    (loop for i below (ring-buffer-count ring)
          collect (svref (ring-buffer-data ring)
                         (mod (+ (ring-buffer-head ring) i) cap)))))

;;; ---------------------------------------------------------------------------
;;; Job objects and registry
;;; ---------------------------------------------------------------------------

(defclass job ()
  ((id         :initarg :id :reader job-id)
   (pipeline   :initarg :pipeline :reader job-pipeline)
   (result     :initform nil :accessor job-result)
   (state      :initform :running :accessor job-state
               :documentation ":running :stopped :parked :done")
   (background :initarg :background :initform t :accessor job-background-p)
   (on-failure :initarg :on-failure :initform :collect :reader job-on-failure)
   (stop-flag  :initarg :stop-flag :reader job-stop-flag)
   (output      :initarg :output :accessor job-output
                :documentation "A bounded ring-buffer of the job's output objects.")
   (buffer-lock :initform (sb-thread:make-mutex :name "job-buffer") :reader job-buffer-lock)
   (sink-thread :initform nil :accessor job-sink-thread)
   (complete    :initform nil :accessor job-complete-p)
   (events   :initform '() :accessor %job-events)     ; newest-first
   (parked   :initform '() :accessor job-parked)      ; parked-worker list
   (lock :initform (sb-thread:make-mutex :name "job") :reader job-lock)
   (cvar :initform (sb-thread:make-waitqueue :name "job") :reader job-cvar))
  (:documentation "A running pipeline under job control (SPEC §6)."))

(defmethod print-object ((j job) stream)
  (print-unreadable-object (j stream :type t)
    (format stream "~D ~A~:[~; bg~]" (job-id j) (job-state j) (job-background-p j))))

(defvar *jobs* (make-hash-table) "Job id -> job.")
(defvar *job-counter* 0)
(defvar *jobs-lock* (sb-thread:make-mutex :name "jobs"))

(defun %next-job-id ()
  (sb-thread:with-mutex (*jobs-lock*) (incf *job-counter*)))

(defun register-job (job)
  (sb-thread:with-mutex (*jobs-lock*) (setf (gethash (job-id job) *jobs*) job)))

(defun find-job (id-or-job)
  "Resolve a job id or job object to a job."
  (if (typep id-or-job 'job)
      id-or-job
      (sb-thread:with-mutex (*jobs-lock*) (gethash id-or-job *jobs*))))

(defun all-jobs ()
  (sb-thread:with-mutex (*jobs-lock*)
    (sort (loop for j being the hash-values of *jobs* collect j) #'< :key #'job-id)))

(defun job-events (job)
  "The job's event lines, oldest first."
  (reverse (%job-events (find-job job))))

;;; ---------------------------------------------------------------------------
;;; Job events (surfaced at the prompt)
;;; ---------------------------------------------------------------------------

(defvar *pending-job-events* '() "Global event queue drained at the prompt.")
(defvar *events-lock* (sb-thread:make-mutex :name "job-events"))

(defun %post-event (job text)
  (sb-thread:with-mutex ((job-lock job)) (push text (%job-events job)))
  (sb-thread:with-mutex (*events-lock*) (push text *pending-job-events*)))

(defun take-job-events ()
  "Return and clear the pending job events (for the prompt to report)."
  (sb-thread:with-mutex (*events-lock*)
    (prog1 (nreverse *pending-job-events*) (setf *pending-job-events* '()))))

;;; ---------------------------------------------------------------------------
;;; Parked workers (cross-thread condition handling)
;;; ---------------------------------------------------------------------------

(defstruct parked-worker
  job label condition thread restarts
  (lock (sb-thread:make-mutex :name "parked"))
  (cvar (sb-thread:make-waitqueue :name "parked"))
  (action nil))

(defun %make-park-hook (job)
  "The *WORKER-ERROR-HOOK* a running job installs in its workers."
  (lambda (label condition) (%park-worker job label condition)))

(defun %park-worker (job label condition)
  "Called (in the worker thread, stack intact) when a worker hits an unhandled
condition.  Records the parked worker, posts an event, and blocks until
debug-job supplies a directive — then invokes the chosen restart here, in the
worker's own dynamic context."
  (let ((pw (make-parked-worker
             :job job :label label :condition condition
             :thread sb-thread:*current-thread*
             :restarts (mapcar #'restart-name (compute-restarts condition)))))
    (sb-thread:with-mutex ((job-lock job))
      (push pw (job-parked job))
      (setf (job-state job) :parked))
    (%post-event job (format nil "job ~D parked: ~A in ~A stage"
                             (job-id job) (type-of condition) label))
    (let ((directive (%await-directive pw)))
      (%unpark job pw)
      (ecase (car directive)
        (:invoke-restart
         (let ((r (find-restart (second directive) condition)))
           (when r (apply #'invoke-restart r (cddr directive)))))
        (:decline nil)))))                 ; return -> condition propagates (worker dies)

(defun %await-directive (pw)
  (sb-thread:with-mutex ((parked-worker-lock pw))
    (loop until (parked-worker-action pw)
          do (sb-thread:condition-wait (parked-worker-cvar pw) (parked-worker-lock pw)))
    (parked-worker-action pw)))

(defun %give-directive (pw directive)
  (sb-thread:with-mutex ((parked-worker-lock pw))
    (setf (parked-worker-action pw) directive)
    (sb-thread:condition-broadcast (parked-worker-cvar pw))))

(defun %unpark (job pw)
  (sb-thread:with-mutex ((job-lock job))
    (setf (job-parked job) (remove pw (job-parked job)))
    (when (and (null (job-parked job)) (eq (job-state job) :parked))
      (setf (job-state job) :running))))

(defun job-restarts (job)
  "The restart names offered by the job's (first) parked worker."
  (let ((pw (first (job-parked (find-job job)))))
    (and pw (parked-worker-restarts pw))))

(defun debug-job (job &key (restart 'use-raw-lines) args)
  "Attach to a parked JOB: invoke RESTART (with ARGS) in the parked worker's own
context, resuming the frozen line.  With no live restart named RESTART the
worker declines and the condition propagates."
  (let* ((j (find-job job))
         (pw (first (job-parked j))))
    (unless pw (error "job ~A is not parked" (and j (job-id j))))
    (%give-directive pw (list* :invoke-restart restart args))
    (%post-event j (format nil "job ~D resumed via ~A" (job-id j) restart))
    j))

(defun resume-job (job)
  "Tell a parked job's worker to decline (let the condition propagate)."
  (let ((pw (first (job-parked (find-job job)))))
    (when pw (%give-directive pw (list :decline)))
    job))

;;; ---------------------------------------------------------------------------
;;; Launching, the output sink, and waiting
;;; ---------------------------------------------------------------------------

(defun run-job (pipeline &key (on-failure :collect) (on-parse-error :use-raw-lines)
                              input (background t)
                              (buffer-capacity *default-job-buffer-capacity*))
  "Run PIPELINE as a job.  The pipeline's channels share the job's stop-flag and
its workers install the park hook, then a sink thread drains output into the
job's bounded ring buffer (BUFFER-CAPACITY objects) so the job progresses in the
background without unbounded memory growth."
  (let* ((sf (make-stop-flag))
         (job (make-instance 'job :id (%next-job-id) :pipeline pipeline
                             :stop-flag sf :on-failure on-failure :background background
                             :output (make-ring-buffer buffer-capacity))))
    (register-job job)
    ;; Bind the shared stop-flag and park hook while the pipeline (and thus its
    ;; channels and worker threads) are created, so both propagate everywhere.
    (let ((*channel-stop-flag* sf)
          (*worker-error-hook* (%make-park-hook job)))
      (setf (job-result job)
            (run-pipeline pipeline :on-failure on-failure
                                   :on-parse-error on-parse-error :input input)))
    (%start-sink job)
    (%post-event job (format nil "job ~D started" (job-id job)))
    job))

(defun %start-sink (job)
  (setf (job-sink-thread job)
        (spawn-stage-thread
         (lambda ()
           (unwind-protect
                (do-object-seq (obj (pipeline-result-seq (job-result job)))
                  (sb-thread:with-mutex ((job-buffer-lock job))
                    (ring-push (job-output job) obj)))
             (%mark-complete job)))
         :name "consh-job-sink")))

(defun %mark-complete (job)
  ;; The pipeline reached EOF: release its fds/streams/threads (idempotent; the
  ;; processes are already gone so killpg just ESRCHs).  Runs in the sink thread,
  ;; which is not among the run-state threads, so it never joins itself.
  (when (job-result job)
    (ignore-errors (%teardown (pipeline-result-state (job-result job)))))
  (sb-thread:with-mutex ((job-lock job))
    (setf (job-complete-p job) t)
    (unless (member (job-state job) '(:stopped :parked))
      (setf (job-state job) :done))
    (sb-thread:condition-broadcast (job-cvar job)))
  (%post-event job (format nil "job ~D done" (job-id job))))

(defun job-output-list (job)
  "A snapshot of the retained output, oldest first."
  (let ((j (find-job job)))
    (sb-thread:with-mutex ((job-buffer-lock j))
      (ring-list (job-output j)))))

(defun job-output-dropped (job)
  "How many output objects the ring buffer has dropped (0 unless the job
overran its buffer capacity)."
  (let ((j (find-job job)))
    (sb-thread:with-mutex ((job-buffer-lock j))
      (ring-buffer-dropped (job-output j)))))

(defun wait-job (job &key timeout)
  "Block until JOB completes, then return (values output-list t).  Under
:on-failure :signal, raises for the first failed stage.  Returns (values nil nil)
on timeout."
  (let ((j (find-job job)))
    (sb-thread:with-mutex ((job-lock j))
      (loop until (job-complete-p j)
            do (unless (sb-thread:condition-wait (job-cvar j) (job-lock j) :timeout timeout)
                 (return-from wait-job (values nil nil)))))
    (let ((result (job-result j)))
      (dolist (rec (run-state-externals (pipeline-result-state result)))
        (wait-process (ext-record-process rec) :timeout 10))
      (when (eq (job-on-failure j) :signal)
        (let ((failure (%first-failure result)))
          (when failure (error failure))))
      (values (job-output-list j) t))))

;;; ---------------------------------------------------------------------------
;;; fg / bg / C-z
;;; ---------------------------------------------------------------------------

(defun %job-pgid (job)
  (let ((result (job-result job)))
    (and result (run-state-pgid (pipeline-result-state result)))))

(defun stop-job (job)
  "C-z: stop the job.  Pause the shared stop-flag (parking Lisp workers at their
next channel op) and SIGTSTP the external process group."
  (let ((j (find-job job)))
    (stop-flag-pause (job-stop-flag j))
    (let ((pgid (%job-pgid j)))
      (when pgid (ignore-errors (c-killpg pgid +sigtstp+))))
    (sb-thread:with-mutex ((job-lock j)) (setf (job-state j) :stopped))
    (%post-event j (format nil "job ~D stopped" (job-id j)))
    j))

(defun continue-job (job)
  "Resume a stopped job: SIGCONT the process group and release the parked Lisp
workers."
  (let ((j (find-job job)))
    (when (eq (job-state j) :stopped)
      (let ((pgid (%job-pgid j)))
        (when pgid (ignore-errors (c-killpg pgid +sigcont+))))
      (stop-flag-resume (job-stop-flag j))
      (sb-thread:with-mutex ((job-lock j)) (setf (job-state j) :running))
      (%post-event j (format nil "job ~D continued" (job-id j))))
    j))

(defun fg (job &key timeout)
  "Foreground JOB: resume it if stopped, then wait for completion and return its
output (SPEC §6; tcsetpgrp is a no-op without a controlling terminal)."
  (let ((j (find-job job)))
    (continue-job j)
    (setf (job-background-p j) nil)
    (wait-job j :timeout timeout)))

(defun bg (job)
  "Background JOB: resume it if stopped and return immediately."
  (let ((j (find-job job)))
    (continue-job j)
    (setf (job-background-p j) t)
    j))

(defun kill-job (job)
  "Terminate JOB and reclaim its processes and threads."
  (let ((j (find-job job)))
    (when (job-result j) (%teardown (pipeline-result-state (job-result j))))
    (sb-thread:with-mutex ((job-lock j))
      (setf (job-state j) :done (job-complete-p j) t)
      (sb-thread:condition-broadcast (job-cvar j)))
    (%post-event j (format nil "job ~D killed" (job-id j)))
    j))
