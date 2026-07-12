;;;; wrappers/cat.lisp — cat: lines of text.
;;;;
;;;; cat has no structure to recover, so it rides the default string-lines
;;;; parse-output.  The wrapper exists so `cat` dispatches to a real invocation
;;;; class (a hook for future enrichment, and to hold parsed operands).

(in-package #:consh)

(defclass cat-invocation (command-invocation)
  ((files :initform nil :accessor cat-files))
  (:documentation "A `cat` call.  Output is lines of text (the default method)."))

(defmethod initialize-instance :after ((c cat-invocation) &key)
  (multiple-value-bind (flags operands) (split-flags (invocation-arguments c))
    (declare (ignore flags))
    (setf (cat-files c) operands)))

(register-wrapper "cat" 'cat-invocation)
