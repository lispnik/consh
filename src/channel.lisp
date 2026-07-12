;;;; channel.lisp — bounded, thread-safe object channels (SPEC.md §4).
;;;;
;;;; A channel carries CLOS objects (or any Lisp value) between threads, not
;;;; bytes.  It is a fixed-capacity FIFO ring buffer guarded by a mutex with two
;;;; condition variables:
;;;;
;;;;   * channel-put blocks while the buffer is full  -> BACKPRESSURE.  This is
;;;;     the object-level analogue of a full 64 KiB kernel pipe: a fast producer
;;;;     parks and memory stays flat.
;;;;   * channel-take blocks while the buffer is empty, and returns the EOF
;;;;     sentinel once the writer end is closed and the buffer is drained.
;;;;
;;;; Two independent closes model the two directions of shutdown:
;;;;
;;;;   * close-channel      — the PRODUCER is done (normal EOF).  Takers drain
;;;;                          what remains, then see +channel-eof+.
;;;;   * close-for-reading  — the CONSUMER cancels (SPEC.md §4 downstream
;;;;                          cancellation).  Blocked/future putters observe a
;;;;                          CHANNEL-CLOSED condition; buffered objects are
;;;;                          dropped so their memory is released.
;;;;
;;;; A channel may reference a STOP-FLAG.  Every put/take checks it at the op
;;;; boundary and parks while paused — the thread-level analogue of SIGTSTP used
;;;; by the C-z path (SPEC.md §6).  Parking uses the stop-flag's own lock, never
;;;; the channel lock, so a paused worker does not wedge the channel.

(in-package #:consh)

;;; ---------------------------------------------------------------------------
;;; EOF sentinel
;;; ---------------------------------------------------------------------------

(defvar +channel-eof+ (make-symbol "CHANNEL-EOF")
  "Unique sentinel returned by CHANNEL-TAKE once a channel is drained and its
writer end is closed.  Distinguishable from any user object by identity.")

(declaim (inline eof-p))
(defun eof-p (x)
  "True iff X is the channel EOF sentinel."
  (eq x +channel-eof+))

;;; ---------------------------------------------------------------------------
;;; Conditions
;;; ---------------------------------------------------------------------------

(define-condition channel-closed (error)
  ((channel :initarg :channel :reader channel-closed-channel)
   (object  :initarg :object  :reader channel-closed-object :initform nil)
   (reason  :initarg :reason  :reader channel-closed-reason :initform :reader-closed))
  (:report (lambda (c s)
             (format s "cannot put onto a closed channel (~A)"
                     (channel-closed-reason c))))
  (:documentation
   "Signalled by CHANNEL-PUT when the object cannot be delivered: either the
consumer called CLOSE-FOR-READING (:reason :reader-closed — downstream
cancellation) or the writer end was already closed (:reason :writer-closed)."))

;;; ---------------------------------------------------------------------------
;;; Stop flag (thread-level pause, the C-z analogue)
;;; ---------------------------------------------------------------------------

(defstruct (stop-flag (:constructor make-stop-flag ()))
  "Shared pause switch for a set of channels.  When paused, threads entering a
channel op park until resumed."
  (lock   (sb-thread:make-mutex :name "stop-flag") :read-only t)
  (cvar   (sb-thread:make-waitqueue :name "stop-flag-resume") :read-only t)
  (paused nil :type boolean))

(defun stop-flag-pause (stop-flag)
  "Pause: threads reaching their next channel op will park."
  (sb-thread:with-mutex ((stop-flag-lock stop-flag))
    (setf (stop-flag-paused stop-flag) t))
  stop-flag)

(defun stop-flag-resume (stop-flag)
  "Resume: release every thread parked on STOP-FLAG."
  (sb-thread:with-mutex ((stop-flag-lock stop-flag))
    (setf (stop-flag-paused stop-flag) nil)
    (sb-thread:condition-broadcast (stop-flag-cvar stop-flag)))
  stop-flag)

(defun stop-flag-paused-p (stop-flag)
  (stop-flag-paused stop-flag))

;;; ---------------------------------------------------------------------------
;;; The channel
;;; ---------------------------------------------------------------------------

(defclass channel ()
  ((buffer   :accessor %buffer
             :documentation "Ring buffer, a simple-vector of CAPACITY slots.")
   (head     :initform 0 :accessor %head :type fixnum)
   (tail     :initform 0 :accessor %tail :type fixnum)
   (count    :initform 0 :accessor %count :type fixnum)
   (capacity :initarg :capacity :reader channel-capacity :type fixnum)
   (writer-closed :initform nil :accessor %writer-closed :type boolean)
   (reader-closed :initform nil :accessor %reader-closed :type boolean)
   (lock      :accessor %lock)
   (not-full  :accessor %not-full)
   (not-empty :accessor %not-empty)
   (stop-flag :initarg :stop-flag :initform nil :reader channel-stop-flag))
  (:documentation
   "A bounded thread-safe FIFO of objects (SPEC.md §4).  Backpressure and EOF
are its cancellation primitives; see the file header."))

(defmethod initialize-instance :after ((c channel) &key)
  (setf (%buffer c)    (make-array (channel-capacity c) :initial-element nil)
        (%lock c)      (sb-thread:make-mutex :name "channel")
        (%not-full c)  (sb-thread:make-waitqueue :name "channel-not-full")
        (%not-empty c) (sb-thread:make-waitqueue :name "channel-not-empty")))

(defvar *channel-stop-flag* nil
  "Default stop-flag for channels created within its dynamic extent.  A running
job binds it so every channel in one pipeline shares a pause switch (the C-z
path parks the whole line through it).")

(defun make-channel (&key (capacity 256) (stop-flag *channel-stop-flag*))
  "Create a channel holding up to CAPACITY objects before producers block.
STOP-FLAG defaults to *CHANNEL-STOP-FLAG*, letting a job pause all its channels."
  (check-type capacity (integer 1))
  (make-instance 'channel :capacity capacity :stop-flag stop-flag))

(defun channelp (x) (typep x 'channel))

(defmethod print-object ((c channel) stream)
  ;; Best-effort snapshot; intentionally lock-free for REPL display.
  (print-unreadable-object (c stream :type t)
    (format stream "~D/~D~:[~; writer-closed~]~:[~; reader-closed~]"
            (%count c) (channel-capacity c)
            (%writer-closed c) (%reader-closed c))))

;;; --- ring buffer helpers (all called with the channel lock held) -----------

(declaim (inline %full-p %empty-p))
(defun %full-p (c) (= (%count c) (channel-capacity c)))
(defun %empty-p (c) (zerop (%count c)))

(defun %ring-push (c object)
  (let ((buf (%buffer c)))
    (setf (svref buf (%tail c)) object
          (%tail c) (mod (1+ (%tail c)) (channel-capacity c)))
    (incf (%count c))))

(defun %ring-pop (c)
  (let* ((buf (%buffer c))
         (object (svref buf (%head c))))
    (setf (svref buf (%head c)) nil        ; drop our reference so it can be GC'd
          (%head c) (mod (1+ (%head c)) (channel-capacity c)))
    (decf (%count c))
    object))

(defun %ring-clear (c)
  (fill (%buffer c) nil)
  (setf (%head c) 0 (%tail c) 0 (%count c) 0))

;;; --- stop-flag parking ------------------------------------------------------

(defun %park-if-stopped (channel)
  "Park the calling thread while CHANNEL's stop-flag is paused.  Uses the
stop-flag's own lock — never the channel lock — so parking one worker does not
block the channel for others."
  (let ((sf (channel-stop-flag channel)))
    (when sf
      (sb-thread:with-mutex ((stop-flag-lock sf))
        (loop while (stop-flag-paused sf)
              do (sb-thread:condition-wait (stop-flag-cvar sf)
                                           (stop-flag-lock sf)))))))

;;; ---------------------------------------------------------------------------
;;; Core operations
;;; ---------------------------------------------------------------------------

(defun channel-put (channel object)
  "Put OBJECT onto CHANNEL, blocking while the channel is full (backpressure).
Signals CHANNEL-CLOSED if the consumer has closed the channel for reading or
the writer end is already closed.  Returns OBJECT."
  (%park-if-stopped channel)
  (sb-thread:with-mutex ((%lock channel))
    (loop while (and (%full-p channel)
                     (not (%reader-closed channel))
                     (not (%writer-closed channel)))
          do (sb-thread:condition-wait (%not-full channel) (%lock channel)))
    (cond
      ((%reader-closed channel)
       (error 'channel-closed :channel channel :object object
                              :reason :reader-closed))
      ((%writer-closed channel)
       (error 'channel-closed :channel channel :object object
                              :reason :writer-closed))
      (t
       (%ring-push channel object)
       (sb-thread:condition-notify (%not-empty channel))
       object))))

(defun channel-take (channel)
  "Take the next object from CHANNEL, blocking while it is empty.  Returns
+CHANNEL-EOF+ once the channel is drained and its writer end is closed (or the
channel was closed for reading)."
  (%park-if-stopped channel)
  (sb-thread:with-mutex ((%lock channel))
    (loop
      (cond
        ((not (%empty-p channel))
         (let ((object (%ring-pop channel)))
           (sb-thread:condition-notify (%not-full channel))
           (return object)))
        ((or (%writer-closed channel) (%reader-closed channel))
         (return +channel-eof+))
        (t
         (sb-thread:condition-wait (%not-empty channel) (%lock channel)))))))

(defun close-channel (channel)
  "Close the WRITER end: no more objects will be put.  Takers drain the buffer
then receive +CHANNEL-EOF+.  Idempotent.  Returns CHANNEL."
  (sb-thread:with-mutex ((%lock channel))
    (setf (%writer-closed channel) t)
    ;; Wake takers so they can observe EOF, and putters so a put racing the
    ;; close observes :writer-closed rather than blocking forever.
    (sb-thread:condition-broadcast (%not-empty channel))
    (sb-thread:condition-broadcast (%not-full channel)))
  channel)

(defun close-for-reading (channel)
  "Downstream cancellation (SPEC.md §4): the CONSUMER is done.  Blocked and
future putters get a CHANNEL-CLOSED condition; buffered objects are dropped.
Takes return +CHANNEL-EOF+.  Idempotent.  Returns CHANNEL."
  (sb-thread:with-mutex ((%lock channel))
    (setf (%reader-closed channel) t)
    (%ring-clear channel)
    (sb-thread:condition-broadcast (%not-full channel))   ; wake putters -> CHANNEL-CLOSED
    (sb-thread:condition-broadcast (%not-empty channel)))  ; wake takers  -> EOF
  channel)

;;; ---------------------------------------------------------------------------
;;; Introspection
;;; ---------------------------------------------------------------------------

(defun channel-count (channel)
  "Number of objects currently buffered."
  (sb-thread:with-mutex ((%lock channel)) (%count channel)))

(defun channel-empty-p (channel)
  (sb-thread:with-mutex ((%lock channel)) (%empty-p channel)))

(defun channel-full-p (channel)
  (sb-thread:with-mutex ((%lock channel)) (%full-p channel)))

(defun channel-writer-closed-p (channel)
  (sb-thread:with-mutex ((%lock channel)) (%writer-closed channel)))

(defun channel-reader-closed-p (channel)
  (sb-thread:with-mutex ((%lock channel)) (%reader-closed channel)))
