;;;; wrappers/wc.lisp — wc: parse the default line/word/byte counts into objects.
;;;;
;;;; With no count-selecting flag, `wc` prints "lines words bytes [file]" — the
;;;; same on GNU and BSD — so parse-output turns each row (including the "total"
;;;; row for multiple files) into a wc-count object.  If the user selects a
;;;; specific count (-l, -w, -c, -m, -L), the column layout changes, so we leave
;;;; wc as plain string lines.

(in-package #:consh)

(defclass wc-count ()
  ((lines :initarg :lines :reader wc-count-lines)
   (words :initarg :words :reader wc-count-words)
   (bytes :initarg :bytes :reader wc-count-bytes)
   (file  :initarg :file  :initform nil :reader wc-count-file
          :documentation "Source file, \"total\" for the summary row, or NIL for stdin."))
  (:documentation "One `wc` row: line, word, and byte counts for a file."))

(defmethod print-object ((c wc-count) stream)
  (print-unreadable-object (c stream :type t)
    (format stream "~@[~A: ~]~Dl ~Dw ~Db"
            (wc-count-file c) (wc-count-lines c) (wc-count-words c) (wc-count-bytes c))))

(defclass wc-invocation (command-invocation) ()
  (:documentation "A `wc` call.  Yields wc-count objects in the default mode."))

(defparameter +wc-count-long-flags+
  '("--lines" "--words" "--bytes" "--chars" "--max-line-length"))

(defun %wc-count-selected-p (command)
  "T if the user asked for a specific count, changing wc's default 3-column
layout."
  (let ((args (invocation-arguments command)))
    (or (intersection '(#\l #\w #\c #\m #\L) (short-flag-chars args))
        (some (lambda (f) (flag-present-p args f)) +wc-count-long-flags+))))

(defun %parse-wc-line (command line)
  "Parse one default `wc` row: lines, words, bytes, and an optional filename."
  (let ((fields (split-whitespace line)))
    (unless (>= (length fields) 3) (signal-parse-error command line))
    (flet ((int (i) (parse-integer (nth i fields) :junk-allowed t)))
      (let ((lines (int 0)) (words (int 1)) (bytes (int 2)))
        (unless (and lines words bytes) (signal-parse-error command line))
        (make-instance 'wc-count :lines lines :words words :bytes bytes
                       :file (when (> (length fields) 3)
                               (join-with-space (nthcdr 3 fields))))))))

(defmethod parse-output ((command wc-invocation) stream
                         &key (on-parse-error :error) &allow-other-keys)
  (if (%wc-count-selected-p command)
      (call-next-method)                 ; a single selected column: string lines
      (emitting (emit :on-parse-error on-parse-error)
        (loop for line = (read-line stream nil nil)
              while line
              when (plusp (length (string-trim '(#\Space #\Tab) line)))
                do (funcall emit
                            (parse-record command line
                              (lambda () (%parse-wc-line command line))))))))

(register-wrapper "wc" 'wc-invocation)
