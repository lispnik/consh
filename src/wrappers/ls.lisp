;;;; wrappers/ls.lisp — ls: parse bare names, enrich each with stat(2).
;;;;
;;;; SPEC.md §2 "Wrappers may ENRICH": ls prints only names, so we parse the
;;;; names and then stat() each in-process to return full file objects.  This is
;;;; the on-ramp to replacing external tools with native implementations without
;;;; changing call sites.  If a listed name cannot be stat'd (it vanished, a
;;;; permission race, ...) we signal PARSE-ERROR so USE-RAW-LINES can fall back
;;;; to the bare name.

(in-package #:consh)

;;; ---------------------------------------------------------------------------
;;; The enriched object
;;; ---------------------------------------------------------------------------

(defclass file-info ()
  ((name  :initarg :name  :reader file-name)
   (path  :initarg :path  :reader file-path)
   (size  :initarg :size  :reader file-size)
   (mtime :initarg :mtime :reader file-mtime :documentation "Epoch seconds.")
   (mode  :initarg :mode  :reader file-mode)
   (uid   :initarg :uid   :reader file-uid)
   (gid   :initarg :gid   :reader file-gid)
   (owner :initarg :owner :reader file-owner :documentation "Login name or NIL."))
  (:documentation "A filesystem entry enriched from a bare ls name via stat(2)."))

(defmethod print-object ((f file-info) stream)
  (print-unreadable-object (f stream :type t)
    (format stream "~A ~D bytes~@[ ~A~]" (file-name f) (file-size f) (file-owner f))))

(defun enrich-file (name directory)
  "Stat NAME relative to DIRECTORY and build a FILE-INFO.  Signals FFI-ERROR (via
STAT-FIELDS) if the entry cannot be stat'd."
  (let ((path (merge-pathnames name directory)))
    (multiple-value-bind (size mtime mode uid gid) (stat-fields path)
      (make-instance 'file-info
                     :name name :path path
                     :size size :mtime mtime :mode mode
                     :uid uid :gid gid
                     :owner (uid-username uid)))))

;;; ---------------------------------------------------------------------------
;;; The invocation
;;; ---------------------------------------------------------------------------

(defclass ls-invocation (command-invocation)
  ((long-p    :initform nil :accessor ls-long-p)
   (paths     :initform nil :accessor ls-paths)
   (directory :initarg :directory :initform nil :accessor %ls-directory
              :documentation "Explicit enrichment base; NIL means derive it."))
  (:documentation "An `ls` call, with its flags parsed into slots."))

(defmethod initialize-instance :after ((c ls-invocation) &key)
  (multiple-value-bind (flags operands) (split-flags (invocation-arguments c))
    (setf (ls-long-p c) (flag-present-p flags "-l" "--long")
          (ls-paths c) (or operands (list ".")))))

(defun %existing-directory (path)
  "PATH's truename if it names an existing directory, else NIL."
  (ignore-errors
   (let ((p (probe-file path)))
     ;; a directory truename has directory components but no name/type
     (and p (null (pathname-name p)) (null (pathname-type p)) p))))

(defun ls-directory (command)
  "The directory whose entries this ls enriches: the explicit :directory, else
the single directory operand, else *current-directory*.  Relative operands are
resolved against *current-directory* — never the process cwd — so consh's
no-chdir model holds (SPEC.md §1)."
  (or (%ls-directory command)
      (let* ((paths (ls-paths command))
             (operand (and (= 1 (length paths)) (first paths))))
        (cond ((null operand) nil)
              ((or (string= operand ".") (string= operand "./")) *current-directory*)
              (t (%existing-directory
                  (merge-pathnames operand *current-directory*)))))
      *current-directory*))

;;; ---------------------------------------------------------------------------
;;; parse-output: names -> enriched file objects
;;; ---------------------------------------------------------------------------

(defmethod parse-output ((command ls-invocation) stream
                         &key (on-parse-error :error) &allow-other-keys)
  (let ((directory (ls-directory command)))
    (emitting (emit :on-parse-error on-parse-error)
      (loop for name = (read-line stream nil nil)
            while name
            when (plusp (length name))
              do (funcall emit
                          (parse-record command name
                            (lambda ()
                              (handler-case (enrich-file name directory)
                                (ffi-error (e)
                                  (signal-parse-error command name e))))))))))

(register-wrapper "ls" 'ls-invocation)
