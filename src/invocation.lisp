;;;; invocation.lisp — command-invocation classes and the wrapper registry.
;;;;
;;;; SPEC.md §2: the parser protocol dispatches on INVOCATION OBJECTS, not on
;;;; command-name symbols — because `ls` and `ls -l` have different shapes.  The
;;;; sugar layer (Phase 6) will build these; for now MAKE-INVOCATION looks up a
;;;; registered class by program name and lets that class parse its own flags.
;;;;
;;;; New wrappers require no change to the shell core: they subclass
;;;; command-invocation, register themselves, and add protocol methods (see
;;;; wrappers/).

(in-package #:consh)

;;; ---------------------------------------------------------------------------
;;; Base invocation
;;; ---------------------------------------------------------------------------

(defclass command-invocation ()
  ((program   :initarg :program   :reader invocation-program :type string)
   (arguments :initarg :arguments :initform nil :reader invocation-arguments)
   (dialect   :initform :unknown  :accessor invocation-dialect
              :documentation "Cached GNU/BSD/version dialect; :unknown until probed."))
  (:documentation
   "One external-command invocation.  The base class parses nothing special and
its output is handled by the default (string-lines) protocol methods."))

(defmethod print-object ((c command-invocation) stream)
  (print-unreadable-object (c stream :type t)
    (format stream "~A~{ ~A~}" (invocation-program c) (invocation-arguments c))))

;;; ---------------------------------------------------------------------------
;;; Flag parsing helper
;;; ---------------------------------------------------------------------------

(defun split-flags (arguments)
  "Partition ARGUMENTS into (values flags operands): tokens beginning with '-'
are flags, the rest are operands.  A lone \"--\" ends flag parsing; everything
after it is an operand.  (Deliberately simple — full getopt is not needed until
the surface layer.)"
  (let ((flags '()) (operands '()) (only-operands nil))
    (dolist (a arguments)
      (cond (only-operands (push a operands))
            ;; non-string args (from Lisp escapes) are always operands
            ((not (stringp a)) (push a operands))
            ((string= a "--") (setf only-operands t))
            ((and (> (length a) 0) (char= (char a 0) #\-) (not (string= a "-")))
             (push a flags))
            (t (push a operands))))
    (values (nreverse flags) (nreverse operands))))

(defun flag-present-p (flags &rest names)
  "True if any of NAMES appears in FLAGS."
  (some (lambda (n) (member n flags :test #'string=)) names))

(defun short-flag-chars (arguments)
  "The set of short-flag characters across ARGUMENTS — every character of each
single-dash bundle (e.g. \"-rn\" -> #\\r #\\n).  Long --options, operands, and a
lone \"-\" are ignored.  Wrappers use this to detect bundled short flags."
  (let ((chars '()))
    (dolist (a arguments chars)
      (when (and (stringp a) (> (length a) 1)
                 (char= (char a 0) #\-)
                 (char/= (char a 1) #\-))         ; not a long --option
        (loop for ch across a for i from 0 when (plusp i) do (pushnew ch chars))))))

(defun short-flag-present-p (arguments char)
  "True if CHAR appears as a short flag anywhere in ARGUMENTS (bundled or not)."
  (member char (short-flag-chars arguments)))

;;; ---------------------------------------------------------------------------
;;; Wrapper registry
;;; ---------------------------------------------------------------------------

(defvar *wrappers* (make-hash-table :test 'equal)
  "Program name (string) -> invocation class name (symbol).")

(defun register-wrapper (name class)
  "Register CLASS (a class name) as the invocation class for program NAME."
  (setf (gethash name *wrappers*) class))

(defun invocation-class-for (name)
  "The invocation class registered for program NAME, or COMMAND-INVOCATION."
  (gethash name *wrappers* 'command-invocation))

(defun make-invocation (program &rest arguments)
  "Build an invocation for PROGRAM with ARGUMENTS, choosing the registered
wrapper class if one exists (else the generic COMMAND-INVOCATION)."
  (make-instance (invocation-class-for program)
                 :program program :arguments arguments))

;;; ---------------------------------------------------------------------------
;;; Dialect probe scaffold (SPEC.md §2)
;;; ---------------------------------------------------------------------------

(defgeneric command-dialect (command)
  (:documentation
   "The command's output dialect (:gnu / :bsd / version keyword / :unknown),
cached on the invocation.  The default returns the cached slot; wrappers whose
parsing differs by dialect override this to probe (see ENSURE-DIALECT and the
stat wrapper).")
  (:method ((command command-invocation))
    (invocation-dialect command)))

(defparameter *dialects* '(:gnu :bsd)
  "The dialects the TRY-DIALECT restart cycles through.")

(defun next-dialect (current)
  "The dialect to try after CURRENT (cyclic); the first one from :unknown."
  (let ((pos (position current *dialects*)))
    (if pos
        (nth (mod (1+ pos) (length *dialects*)) *dialects*)
        (first *dialects*))))
