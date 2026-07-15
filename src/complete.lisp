;;;; complete.lisp — Tab completion: the COMPLETE generic + context picker.
;;;;
;;;; Split out of surface.lisp.  Loaded after surface (uses *builtins*/*aliases*/
;;;; *wrappers* and the shared path/string helpers) and before lineedit.

(in-package #:consh)

;;; ---------------------------------------------------------------------------
;;; Completion (SPEC §1: completion is a generic function)
;;; ---------------------------------------------------------------------------

(defun %prefixp (prefix string)
  (and (<= (length prefix) (length string))
       (string= prefix string :end2 (length prefix))))

(defgeneric complete (kind text &key &allow-other-keys)
  (:documentation
   "Return a sorted list of completions for TEXT in context KIND — one of
:command (registered wrappers + PATH), :symbol (Lisp symbols), :path (files
under a directory).  New contexts are new methods."))

(defun %path-commands (prefix)
  "Executable names on $PATH beginning with PREFIX."
  (let ((path (sb-ext:posix-getenv "PATH"))
        (names '()))
    (when path
      (dolist (dir (%split-string path #\:))
        (when (plusp (length dir))
          (dolist (p (ignore-errors
                      (directory (merge-pathnames (make-pathname :name :wild :type :wild)
                                                  (%as-directory dir)))))
            (let ((name (file-namestring p)))
              (when (and (plusp (length name)) (%prefixp prefix name))
                (push name names)))))))
    names))
(defmethod complete ((kind (eql :command)) text &key)
  (let ((names '()))
    (flet ((collect (table)
             (maphash (lambda (k v) (declare (ignore v))
                        (when (%prefixp text k) (push k names)))
                      table)))
      (collect *wrappers*) (collect *builtins*) (collect *aliases*))
    (sort (remove-duplicates (append names (ignore-errors (%path-commands text)))
                             :test #'string=)
          #'string<)))

(defmethod complete ((kind (eql :env)) text &key)
  "Environment variable names beginning with TEXT (given without the leading $)."
  (let ((out '()))
    (dolist (entry (sb-ext:posix-environ))
      (let* ((eq (position #\= entry))
             (name (if eq (subseq entry 0 eq) entry)))
        (when (%prefixp text name) (push name out))))
    (sort (remove-duplicates out :test #'string=) #'string<)))

(defmethod complete ((kind (eql :symbol)) text &key (package *package*))
  (let ((up (string-upcase text)) (out '()))
    (do-symbols (s package)
      (when (%prefixp up (symbol-name s))
        (push (string-downcase (symbol-name s)) out)))
    (sort (remove-duplicates out :test #'string=) #'string<)))

(defmethod complete ((kind (eql :path)) text &key (directory *current-directory*))
  (let* ((slash (position #\/ text :from-end t))
         (subdir (if slash (subseq text 0 (1+ slash)) ""))
         (prefix (if slash (subseq text (1+ slash)) text))
         ;; expand a leading ~ for the lookup, but keep the user's ~ in results
         (base (merge-pathnames (if (plusp (length subdir)) (%expand-tilde subdir) "")
                                directory))
         (entries (ignore-errors
                   (directory (merge-pathnames (make-pathname :name :wild :type :wild) base)))))
    (sort
     (loop for p in entries
           for name = (%entry-name p)
           when (%prefixp prefix name)
             collect (concatenate 'string subdir name))
     #'string<)))

(defun %entry-name (pathname)
  "The final component of PATHNAME, with a trailing / for directories."
  (if (and (null (pathname-name pathname)) (pathname-directory pathname))
      (concatenate 'string (car (last (pathname-directory pathname))) "/")
      (file-namestring pathname)))

(defun complete-line (line)
  "Complete the last token of LINE, choosing the context: the first word is a
command, a `,`/`(`-led token is a symbol, otherwise a path."
  (let* ((trimmed (string-left-trim '(#\Space #\Tab) line))
         (last-space (position-if (lambda (c) (member c '(#\Space #\Tab #\|)))
                                  trimmed :from-end t))
         (token (if last-space (subseq trimmed (1+ last-space)) trimmed)))
    (cond ;; $VAR — environment variable names (but not a $(...) Lisp escape)
          ((and (plusp (length token)) (char= (char token 0) #\$)
                (not (and (> (length token) 1) (char= (char token 1) #\())))
           (mapcar (lambda (n) (concatenate 'string "$" n))
                   (complete :env (subseq token 1))))
          ((null last-space) (complete :command token))
          ((and (plusp (length token)) (member (char token 0) '(#\, #\()))
           (complete :symbol (string-left-trim ",(" token)))
          (t (complete :path token)))))
