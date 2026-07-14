;;;; present.lisp — a presentation layer: render object streams as aligned tables.
;;;;
;;;; The REPL prints an object per line via its print-object, which is fine for a
;;;; glance but not for scanning a column.  `table` renders a list of objects as
;;;; an aligned text table.  Each wrapped object type advertises its columns via
;;;; the TABLE-COLUMNS generic — a (header . accessor) list — so
;;;;   (table (pipeline-collect (pipe (ls))))
;;;; prints a NAME/SIZE/OWNER grid.  Numeric columns right-align automatically.

(in-package #:consh)

(defgeneric table-columns (object)
  (:documentation
   "A list of (HEADER . ACCESSOR) describing how to tabulate OBJECT's type.
ACCESSOR is a function of one object returning that column's value.  Define a
method per object type; `table` consults the first row's type.")
  (:method ((object t))
    ;; Fallback: a single column holding the printed object.
    (list (cons "VALUE" #'identity))))

(defmethod table-columns ((o file-info))
  (list (cons "NAME" #'file-name) (cons "SIZE" #'file-size) (cons "OWNER" #'file-owner)))

(defmethod table-columns ((o grep-match))
  (list (cons "FILE" #'grep-match-file) (cons "LINE" #'grep-match-line-number)
        (cons "TEXT" #'grep-match-text)))

(defmethod table-columns ((o block-device))
  (list (cons "NAME" #'block-device-name) (cons "TYPE" #'block-device-type)
        (cons "SIZE" #'block-device-size) (cons "MOUNT" #'block-device-mountpoint)))

(defmethod table-columns ((o ps-process))
  (list (cons "PID" #'ps-process-pid) (cons "USER" #'ps-process-user)
        (cons "RSS" #'ps-process-rss) (cons "STAT" #'ps-process-state)
        (cons "COMMAND" #'ps-process-command)))

(defmethod table-columns ((o git-status))
  (list (cons "CODE" #'git-status-code) (cons "PATH" #'git-status-path)))

(defmethod table-columns ((o filesystem))
  (list (cons "MOUNT" #'filesystem-mount-point) (cons "BLOCKS" #'filesystem-blocks)
        (cons "USED" #'filesystem-used) (cons "AVAIL" #'filesystem-available)
        (cons "USE%" #'filesystem-capacity) (cons "DEVICE" #'filesystem-device)))

(defmethod table-columns ((o wc-count))
  (list (cons "FILE" #'wc-count-file) (cons "LINES" #'wc-count-lines)
        (cons "WORDS" #'wc-count-words) (cons "BYTES" #'wc-count-bytes)))

(defmethod table-columns ((o du-entry))
  (list (cons "BLOCKS" #'du-entry-blocks) (cons "PATH" #'du-entry-path)))

;;; ---------------------------------------------------------------------------
;;; The renderer
;;; ---------------------------------------------------------------------------

(defun %table-cell (value)
  "Render one cell VALUE to a display string."
  (cond ((null value) "")
        ((stringp value) value)
        ((pathnamep value) (namestring value))
        (t (princ-to-string value))))

(defun %table-print-row (stream cells widths aligns &optional bold)
  (let* ((parts (loop for cell in cells and w in widths and al in aligns
                      collect (if (eq al :right)
                                  (format nil "~V@A" w cell)
                                  (format nil "~VA" w cell))))
         ;; right-trim so a left-aligned final column leaves no trailing padding
         (line  (string-right-trim '(#\Space) (format nil "~{~A~^  ~}" parts))))
    (write-string (if bold (format nil "~C[1m~A~C[0m" #\Escape line #\Escape) line)
                  stream)
    (terpri stream)))

(defun table (objects &key (stream *standard-output*) columns color)
  "Print OBJECTS (a list, or a single object) as an aligned text table, using
their TABLE-COLUMNS or an explicit COLUMNS spec (a (header . accessor) list).
Numeric columns right-align.  When COLOR is true, the header and rule are emitted
bold (for a terminal).  Returns no values, so it reads cleanly as the last form
at the REPL."
  (let ((objects (if (listp objects) objects (list objects))))
    (when objects
      (let* ((specs     (or columns (table-columns (first objects))))
             (headers   (mapcar #'car specs))
             (accessors (mapcar #'cdr specs))
             (rows      (mapcar (lambda (o)
                                  (mapcar (lambda (a) (%table-cell (funcall a o))) accessors))
                                objects))
             (widths    (loop for i below (length headers)
                              collect (loop for r in (cons headers rows)
                                            maximize (length (nth i r)))))
             (aligns    (loop for a in accessors
                              collect (if (every (lambda (o) (numberp (funcall a o))) objects)
                                          :right :left)))
             (rules     (mapcar (lambda (w) (make-string w :initial-element #\-)) widths)))
        (%table-print-row stream headers widths aligns color)
        (%table-print-row stream rules   widths aligns color)
        (dolist (r rows) (%table-print-row stream r widths aligns)))))
  (values))
