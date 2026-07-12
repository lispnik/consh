;;;; wrappers/find.lisp — find: yield pathname objects, prefer -print0.
;;;;
;;;; SPEC.md §2 format preference: NUL-delimited output beats newline-scraping
;;;; (paths may contain newlines).  rewrite-invocation adds -print0 so the
;;;; executor requests the machine-readable form; parse-output then splits on
;;;; NUL.  Without -print0 we fall back to line-splitting.

(in-package #:consh)

(defclass find-invocation (command-invocation)
  ((print0 :initarg :print0 :initform nil :reader find-print0-p)
   (start  :initform "."    :accessor find-start))
  (:documentation "A `find` call.  Yields pathname objects."))

(defmethod initialize-instance :after ((c find-invocation) &key)
  (multiple-value-bind (flags operands) (split-flags (invocation-arguments c))
    (when (flag-present-p flags "-print0")
      (setf (slot-value c 'print0) t))
    (when operands
      (setf (find-start c) (first operands)))))

(defun %read-delimited (stream delimiter emit)
  "Call EMIT on each DELIMITER-separated record of STREAM.  A final unterminated
record is emitted too.  Empty records are skipped."
  (let ((buffer (make-string-output-stream)))
    (loop for ch = (read-char stream nil nil) do
      (cond
        ((null ch)
         (let ((rec (get-output-stream-string buffer)))
           (when (plusp (length rec)) (funcall emit rec)))
         (return))
        ((char= ch delimiter)
         (let ((rec (get-output-stream-string buffer)))
           (when (plusp (length rec)) (funcall emit rec))))
        (t (write-char ch buffer))))))

(defmethod parse-output ((command find-invocation) stream
                         &key (on-parse-error :error) &allow-other-keys)
  (emitting (emit :on-parse-error on-parse-error)
    (flet ((emit-path (record) (funcall emit (pathname record))))
      (if (find-print0-p command)
          (%read-delimited stream #\Nul #'emit-path)
          (loop for line = (read-line stream nil nil)
                while line
                when (plusp (length line)) do (emit-path line))))))

(defmethod rewrite-invocation ((command find-invocation))
  "Request NUL-delimited output if the call did not already ask for it."
  (if (find-print0-p command)
      command
      (make-instance 'find-invocation
                     :program (invocation-program command)
                     :arguments (append (invocation-arguments command) '("-print0"))
                     :print0 t)))

(register-wrapper "find" 'find-invocation)
