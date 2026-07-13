;;;; wrappers/find.lisp — find: yield stat-enriched file objects, prefer -print0.
;;;;
;;;; SPEC.md §2 format preference: NUL-delimited output beats newline-scraping
;;;; (paths may contain newlines).  rewrite-invocation adds -print0 so the
;;;; executor requests the machine-readable form; parse-output then splits on
;;;; NUL.  Without -print0 we fall back to line-splitting.
;;;;
;;;; SPEC.md §2 "Wrappers may ENRICH": like ls, find stat(2)s each path it prints
;;;; and yields a FILE-INFO, so downstream stages can filter by size/mtime/owner
;;;; instead of re-shelling `find -size`.  Enrichment is best-effort: find already
;;;; located the entry, so if the stat fails (a race removed it, a permission
;;;; wall, a broken symlink) we degrade to the bare pathname rather than abort the
;;;; traversal.

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

(defun %enrich-found-path (record base)
  "Enrich the found path RECORD (a namestring) into a FILE-INFO via stat(2),
resolving relative paths against BASE.  On stat failure fall back to the bare
pathname — find already found it, so enrichment is best-effort."
  (handler-case (enrich-file record base)
    (ffi-error () (pathname record))))

(defmethod parse-output ((command find-invocation) stream
                         &key (on-parse-error :error) &allow-other-keys)
  ;; find never signals PARSE-ERROR (enrichment degrades to a pathname instead),
  ;; so ON-PARSE-ERROR is inert here; accepted for protocol conformity.
  (declare (ignore on-parse-error))
  (let ((base *current-directory*))
    (emitting (emit)
      (flet ((emit-path (record) (funcall emit (%enrich-found-path record base))))
        (if (find-print0-p command)
            (%read-delimited stream #\Nul #'emit-path)
            (loop for line = (read-line stream nil nil)
                  while line
                  when (plusp (length line)) do (emit-path line)))))))

(defmethod rewrite-invocation ((command find-invocation))
  "Request NUL-delimited output if the call did not already ask for it."
  (if (find-print0-p command)
      command
      (make-instance 'find-invocation
                     :program (invocation-program command)
                     :arguments (append (invocation-arguments command) '("-print0"))
                     :print0 t)))

(register-wrapper "find" 'find-invocation)
