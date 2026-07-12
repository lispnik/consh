;;;; wrappers/grep.lisp — grep: matching lines, with a smarter exit-code map.
;;;;
;;;; grep's output is matching lines (the default string-lines parse-output).
;;;; Its exit codes, though, are non-obvious: 0 = matches found, 1 = NO matches
;;;; (not a failure!), >=2 = real error.  parse-error-output encodes that so a
;;;; "grep found nothing" pipeline does not raise (SPEC.md §2 status translation).

(in-package #:consh)

(defclass grep-invocation (command-invocation)
  ((pattern :initform nil :accessor grep-pattern)
   (files   :initform nil :accessor grep-files))
  (:documentation "A `grep` call.  Yields matching lines as strings."))

(defmethod initialize-instance :after ((c grep-invocation) &key)
  (multiple-value-bind (flags operands) (split-flags (invocation-arguments c))
    (declare (ignore flags))
    (when operands
      (setf (grep-pattern c) (first operands)
            (grep-files c) (rest operands)))))

(defmethod parse-error-output ((command grep-invocation) stream status)
  "grep exit 1 means \"no lines matched\" — benign, so return NIL.  0 is also
success.  status >= 2 is a genuine error."
  (cond
    ((and (integerp status) (<= status 1)) nil)   ; 0 = matched, 1 = no match
    (t (make-condition 'command-failed
                       :command command :status status
                       :stderr (slurp-stream stream)))))

(register-wrapper "grep" 'grep-invocation)
