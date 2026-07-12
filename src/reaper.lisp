;;;; reaper.lisp — process objects and the single waitpid reaper thread.
;;;;
;;;; SPEC.md §5.5: "a single long-lived thread owning waitpid for the image."
;;;; The spec's end-state wakes the reaper via SIGCHLD/self-pipe; for Phase 1 we
;;;; use a condition variable with a short timeout as the wakeup, which keeps all
;;;; waitpid traffic on one thread and needs no signal handling.  No Lisp runs in
;;;; a signal handler.  Processes never linger as zombies: the reaper drains
;;;; every ready child on each wakeup.

(in-package #:consh)

;;; ---------------------------------------------------------------------------
;;; Shell-global dynamic state
;;; ---------------------------------------------------------------------------

(defvar *current-directory*
  (truename *default-pathname-defaults*)
  "The shell's notion of the working directory.  Spawning resolves relative
paths against this via a spawn-time chdir file action; the process-wide cwd is
never changed (SPEC.md §1).")

;;; ---------------------------------------------------------------------------
;;; Process objects
;;; ---------------------------------------------------------------------------

(defclass process ()
  ((pid :initarg :pid :reader process-pid)
   (command :initarg :command :initform nil :reader process-command
            :documentation "The (program . arguments) this process runs, for display.")
   (status :initform :running :accessor process-status
           :documentation "One of :RUNNING :EXITED :SIGNALED :STOPPED.")
   (exit-code :initform nil :accessor process-exit-code
              :documentation "Exit code once :EXITED, else NIL.")
   (term-signal :initform nil :accessor process-term-signal
                :documentation "Terminating signal once :SIGNALED, else NIL.")
   (stop-signal :initform nil :accessor process-stop-signal
                :documentation "Stopping signal while :STOPPED, else NIL.")
   (lock :initform (sb-thread:make-mutex :name "process") :reader %process-lock)
   (cvar :initform (sb-thread:make-waitqueue :name "process-status") :reader %process-cvar))
  (:documentation
   "A spawned subprocess.  Inspectable, signalable, and holdable in a variable
(SPEC.md §6).  Status is updated by the reaper thread and readers block on CVAR
until it changes."))

(defun processp (x) (typep x 'process))

(defmethod print-object ((p process) stream)
  (print-unreadable-object (p stream :type t :identity nil)
    (format stream "pid ~D ~A~@[ ~A~]~@[ cmd ~S~]"
            (process-pid p)
            (process-status p)
            (case (process-status p)
              (:exited (process-exit-code p))
              (:signaled (format nil "sig ~D" (process-term-signal p)))
              (:stopped (format nil "sig ~D" (process-stop-signal p)))
              (t nil))
            (process-command p))))

(defun process-live-p (process)
  "True while PROCESS has not exited or been killed (running or merely stopped)."
  (member (process-status process) '(:running :stopped)))

(defun process-exited-p (process)
  "True once PROCESS has terminated (exited or killed by signal)."
  (member (process-status process) '(:exited :signaled)))

(defun process-success-p (process)
  "True iff PROCESS exited with code 0."
  (and (eq (process-status process) :exited)
       (eql (process-exit-code process) 0)))

;;; ---------------------------------------------------------------------------
;;; Process registry (pid -> process) shared with the reaper
;;; ---------------------------------------------------------------------------

(defvar *registry* (make-hash-table :test 'eql)
  "Live pid -> process map.  Entries are removed once the process is reaped.")
(defvar *registry-lock* (sb-thread:make-mutex :name "process-registry"))

(defun register-process (process)
  (sb-thread:with-mutex (*registry-lock*)
    (setf (gethash (process-pid process) *registry*) process)))

(defun %lookup-process (pid)
  (sb-thread:with-mutex (*registry-lock*)
    (gethash pid *registry*)))

(defun live-process-count ()
  "Number of not-yet-reaped processes consh is tracking."
  (sb-thread:with-mutex (*registry-lock*)
    (hash-table-count *registry*)))

(defun %apply-status (process kind code)
  "Store a decoded wait result on PROCESS and wake its waiters."
  (sb-thread:with-mutex ((%process-lock process))
    (ecase kind
      (:exited   (setf (process-status process) :exited
                       (process-exit-code process) code
                       (process-stop-signal process) nil))
      (:signaled (setf (process-status process) :signaled
                       (process-term-signal process) code
                       (process-stop-signal process) nil))
      (:stopped  (setf (process-status process) :stopped
                       (process-stop-signal process) code))
      (:continued (setf (process-status process) :running
                        (process-stop-signal process) nil)))
    (sb-thread:condition-broadcast (%process-cvar process))))

;;; ---------------------------------------------------------------------------
;;; The reaper thread
;;; ---------------------------------------------------------------------------

(defvar *reaper-thread* nil)
(defvar *reaper-lock* (sb-thread:make-mutex :name "reaper"))
(defvar *reaper-cvar* (sb-thread:make-waitqueue :name "reaper-wakeup"))
(defvar *reaper-stop* nil)
(defparameter *reaper-poll-seconds* 0.05
  "Fallback wakeup interval.  A newly spawned process nudges the reaper, so this
only bounds latency for children that exit before the reaper next parks.")

(defun reaper-running-p ()
  (and *reaper-thread* (sb-thread:thread-alive-p *reaper-thread*)))

(defun notify-reaper ()
  "Wake the reaper so it drains any ready children promptly."
  (sb-thread:with-mutex (*reaper-lock*)
    (sb-thread:condition-notify *reaper-cvar*)))

(defun %drain-ready-children ()
  "Reap every child currently ready, updating process objects.  Returns the
number reaped.  Never blocks (WNOHANG)."
  (let ((count 0))
    (loop
      (multiple-value-bind (pid raw errno)
          (c-waitpid -1 (logior +wnohang+ +wuntraced+))
        (declare (ignorable errno))
        (cond
          ((> pid 0)
           (multiple-value-bind (kind code) (decode-wait-status raw)
             (let ((process (%lookup-process pid)))
               (when process
                 (%apply-status process kind code)
                 ;; Remove from the registry only once it has truly terminated;
                 ;; a stopped child is still ours to track.
                 (when (member kind '(:exited :signaled))
                   (sb-thread:with-mutex (*registry-lock*)
                     (remhash pid *registry*)))))
             (incf count)))
          ;; pid 0: nothing ready right now.  pid -1: no children (ECHILD) or
          ;; another benign error — either way stop draining this round.
          (t
           (return)))))
    count))

(defun %reaper-loop ()
  (loop
    (%drain-ready-children)
    (sb-thread:with-mutex (*reaper-lock*)
      (when *reaper-stop* (return))
      ;; Park until nudged or the poll interval elapses, then loop and drain.
      (sb-thread:condition-wait *reaper-cvar* *reaper-lock*
                                :timeout *reaper-poll-seconds*))))

(defun start-reaper ()
  "Start the reaper thread if it is not already running.  Returns the thread."
  (sb-thread:with-mutex (*reaper-lock*)
    (unless (and *reaper-thread* (sb-thread:thread-alive-p *reaper-thread*))
      (setf *reaper-stop* nil
            *reaper-thread*
            (sb-thread:make-thread #'%reaper-loop :name "consh-reaper"))))
  *reaper-thread*)

(defun ensure-reaper ()
  "Idempotently guarantee the reaper is running."
  (unless (reaper-running-p) (start-reaper))
  *reaper-thread*)

(defun stop-reaper ()
  "Ask the reaper to stop after its next drain and join it."
  (let ((thread *reaper-thread*))
    (when (and thread (sb-thread:thread-alive-p thread))
      (sb-thread:with-mutex (*reaper-lock*)
        (setf *reaper-stop* t)
        (sb-thread:condition-notify *reaper-cvar*))
      (sb-thread:join-thread thread :default nil))
    (setf *reaper-thread* nil)
    t))

;;; ---------------------------------------------------------------------------
;;; Launching and waiting at the shell level
;;; ---------------------------------------------------------------------------

(defun launch (program &optional arguments
               &key (directory *current-directory*) environment (search t) pgid
                    file-actions)
  "Spawn PROGRAM with ARGUMENTS, register a PROCESS object, and return it.
Ensures the reaper is running so the child cannot become a zombie.  DIRECTORY
defaults to *CURRENT-DIRECTORY* so relative programs resolve against the shell's
cwd with no process-wide chdir."
  (ensure-reaper)
  ;; Register before the child can possibly exit and be reaped, so the reaper
  ;; never sees a pid it has no process object for.  We hold the registry lock
  ;; across the spawn to close that race: the reaper cannot remove an entry it
  ;; has not yet added.
  (sb-thread:with-mutex (*registry-lock*)
    (let* ((pid (spawn program arguments
                       :directory directory :environment environment
                       :search search :pgid pgid :file-actions file-actions))
           (process (make-instance 'process :pid pid
                                            :command (cons program arguments))))
      (setf (gethash pid *registry*) process)
      (notify-reaper)
      process)))

(defun wait-process (process &key timeout)
  "Block until PROCESS terminates (exited or signaled), or until TIMEOUT seconds
elapse.  Returns PROCESS if it has terminated, NIL on timeout.  A stopped
process is not terminated, so waiting continues past a stop."
  (ensure-reaper)
  (sb-thread:with-mutex ((%process-lock process))
    (loop until (process-exited-p process)
          do (unless (sb-thread:condition-wait
                      (%process-cvar process) (%process-lock process)
                      :timeout timeout)
               (return-from wait-process (and (process-exited-p process) process)))))
  process)

(defun process-wait (process &key timeout)
  "Alias for WAIT-PROCESS."
  (wait-process process :timeout timeout))

(defun process-status-changed (process previous &key timeout)
  "Block until PROCESS's status differs from PREVIOUS (or TIMEOUT).  Returns the
new status."
  (sb-thread:with-mutex ((%process-lock process))
    (loop while (eq (process-status process) previous)
          do (unless (sb-thread:condition-wait
                      (%process-cvar process) (%process-lock process)
                      :timeout timeout)
               (return))))
  (process-status process))

(defun any-children-p ()
  "True if the kernel still reports children for this process.  Uses a
non-reaping WNOHANG probe: returns NIL exactly when waitpid reports ECHILD.
Intended for tests asserting no zombies remain."
  (multiple-value-bind (pid raw errno) (c-waitpid -1 (logior +wnohang+ +wuntraced+))
    (declare (ignore raw))
    (cond ((> pid 0) t)          ; a child was actually ready (and we just reaped it!)
          ((= pid 0) t)          ; children exist but none ready
          (t (/= errno +echild+)))))

;;; ---------------------------------------------------------------------------
;;; Signalling processes
;;; ---------------------------------------------------------------------------

(defun signal-process (process signal)
  "Send SIGNAL (a number) to PROCESS."
  (c-kill (process-pid process) signal))

(defun signal-process-group (process signal)
  "Send SIGNAL to PROCESS's process group."
  (c-killpg (c-getpgid (process-pid process)) signal))

(defun terminate-process (process)
  "SIGTERM PROCESS."
  (signal-process process +sigterm+))

(defun kill-process (process)
  "SIGKILL PROCESS."
  (signal-process process +sigkill+))
