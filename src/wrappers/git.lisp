;;;; wrappers/git.lisp — git status: parse --porcelain into status objects.
;;;;
;;;; SPEC §2 format preference puts porcelain modes near the top: `git status`
;;;; has a stable, machine-readable `--porcelain` output, so the wrapper rewrites
;;;; `git status` to request it and parses each "XY PATH" record into a
;;;; git-status object (with rename handling).  Other git subcommands fall back
;;;; to the default line parser.

(in-package #:consh)

;;; ---------------------------------------------------------------------------
;;; The status object
;;; ---------------------------------------------------------------------------

(defclass git-status ()
  ((code :initarg :code :reader git-status-code
         :documentation "The two-character porcelain XY status, e.g. \" M\", \"??\", \"R \".")
   (path :initarg :path :reader git-status-path)
   (orig-path :initarg :orig-path :initform nil :reader git-status-orig-path
              :documentation "The source path of a rename/copy, else NIL."))
  (:documentation "One entry of `git status --porcelain`."))

(defun git-status-index-char (s)    (char (git-status-code s) 0))  ; staged side
(defun git-status-worktree-char (s) (char (git-status-code s) 1))  ; worktree side
(defun git-status-untracked-p (s) (string= (git-status-code s) "??"))
(defun git-status-ignored-p (s)   (string= (git-status-code s) "!!"))
(defun git-status-staged-p (s)
  (not (member (git-status-index-char s) '(#\Space #\? #\!))))
(defun git-status-unstaged-p (s)
  (not (member (git-status-worktree-char s) '(#\Space #\? #\!))))

(defmethod print-object ((s git-status) stream)
  (print-unreadable-object (s stream :type t)
    (format stream "~S ~A~@[ <- ~A~]"
            (git-status-code s) (git-status-path s) (git-status-orig-path s))))

;;; ---------------------------------------------------------------------------
;;; The invocation
;;; ---------------------------------------------------------------------------

(defclass git-invocation (command-invocation)
  ((subcommand :initform nil :accessor git-subcommand))
  (:documentation "A `git` call; `git status` yields git-status objects."))

(defmethod initialize-instance :after ((c git-invocation) &key)
  (multiple-value-bind (flags operands) (split-flags (invocation-arguments c))
    (declare (ignore flags))
    (setf (git-subcommand c) (first operands))))

(defun %git-status-p (c) (equal (git-subcommand c) "status"))

(defmethod rewrite-invocation ((c git-invocation))
  "Request porcelain output for `git status` (idempotent); leave other
subcommands alone."
  (if (and (%git-status-p c)
           (not (flag-present-p (invocation-arguments c) "--porcelain")))
      (make-instance 'git-invocation
                     :program "git"
                     :arguments (append (invocation-arguments c) '("--porcelain")))
      c))

(defun %parse-git-status-line (command line)
  "Parse a porcelain-v1 record \"XY PATH\" (PATH may be \"ORIG -> NEW\" for a
rename) into a git-status."
  (when (< (length line) 4) (signal-parse-error command line))
  (let* ((code (subseq line 0 2))
         (rest (subseq line 3))                     ; char 2 is the separating space
         (arrow (search " -> " rest)))
    (make-instance 'git-status
                   :code code
                   :path (if arrow (subseq rest (+ arrow 4)) rest)
                   :orig-path (and arrow (subseq rest 0 arrow)))))

(defmethod parse-output ((c git-invocation) stream
                         &key (on-parse-error :error) &allow-other-keys)
  (if (%git-status-p c)
      (emitting (emit :on-parse-error on-parse-error)
        (loop for line = (read-line stream nil nil)
              while line
              when (plusp (length line))
                do (funcall emit (parse-record c line
                                   (lambda () (%parse-git-status-line c line))))))
      (call-next-method)))                          ; other subcommands: default lines

(register-wrapper "git" 'git-invocation)
