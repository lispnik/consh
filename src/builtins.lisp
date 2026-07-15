;;;; builtins.lisp — the shell builtins (cd, pushd, help, type, source, ...).
;;;;
;;;; Split out of surface.lisp.  The builtin machinery (*builtins*, define-builtin,
;;;; builtin-p) stays in surface; these are the definitions, registered at load
;;;; time.  Loaded after surface.

;;; ---------------------------------------------------------------------------
;;; Builtins (SPEC §1: the user environment is the image)
;;; ---------------------------------------------------------------------------

(in-package #:consh)

(defun %job-spec->job (spec)
  "Resolve a job spec (\"%2\", \"2\", or NIL for the most recent) to a job."
  (if (null spec)
      (car (last (all-jobs)))
      (let ((id (parse-integer (string-left-trim "%" spec) :junk-allowed t)))
        (and id (find-job id)))))

(defun %cd-to (dir)
  "Switch *current-directory* to DIR (resolved against the current directory),
recording *previous-directory*.  Signals a shell-parse-error if it doesn't exist.
Returns the new truename."
  (let ((new (handler-case
                 (truename (merge-pathnames (%ensure-directory-pathname dir)
                                            *current-directory*))
               (file-error () (error 'shell-parse-error :line
                                     (format nil "cd: no such directory: ~A" dir))))))
    (setf *previous-directory* *current-directory*
          *current-directory* new)
    new))

(defun %cdpath-search (arg)
  "If ARG is a bare relative name (no `/`, not `.`/`..`) that isn't present in the
current directory but is found under a $CDPATH entry, return that path; else NIL."
  (unless (or (find #\/ arg) (string= arg ".") (string= arg ".."))
    (let ((cdpath (sb-ext:posix-getenv "CDPATH")))
      (when cdpath
        (loop for dir in (%split-string cdpath #\:)
              thereis (and (plusp (length dir))
                           (let ((p (%safe-probe (merge-pathnames (%ensure-directory-pathname arg)
                                                                 (%as-directory dir)))))
                             (and p (namestring p)))))))))

(define-builtin "cd"
  (lambda (args)
    (let* ((arg (first args))
           (target
             (cond ((null arg) (namestring (user-homedir-pathname)))
                   ((and (string= arg "-") *previous-directory*)
                    (namestring *previous-directory*))
                   ;; a bare name not in cwd may live under $CDPATH
                   ((and (not (%safe-probe (merge-pathnames (%ensure-directory-pathname arg)
                                                           *current-directory*)))
                         (%cdpath-search arg)))
                   (t arg))))
      (namestring (%cd-to target)))))

(define-builtin "pwd" (lambda (args) (declare (ignore args))
                        (namestring *current-directory*)))

(define-builtin "exit"
  (lambda (args)
    (error 'shell-exit :code (if args (or (parse-integer (first args) :junk-allowed t) 0) 0))))

(define-builtin "export"
  (lambda (args)
    (dolist (a args)
      (let ((eq (position #\= a)))
        (when eq (sb-posix:setenv (subseq a 0 eq) (subseq a (1+ eq)) 1))))
    (values)))

(define-builtin "unset"
  (lambda (args) (dolist (a args) (ignore-errors (sb-posix:unsetenv a))) (values)))

(define-builtin "alias"
  (lambda (args)
    (if args
        (progn (dolist (a args)
                 (let ((eq (position #\= a)))
                   (when eq (define-alias (subseq a 0 eq) (subseq a (1+ eq))))))
               (values))
        (loop for k being the hash-keys of *aliases* using (hash-value v)
              collect (format nil "~A=~A" k v)))))

(define-builtin "unalias"
  (lambda (args) (dolist (a args) (remove-alias a)) (values)))

(define-builtin "jobs" (lambda (args) (declare (ignore args)) (all-jobs)))
(define-builtin "fg" (lambda (args) (fg (%job-spec->job (first args)))))
(define-builtin "bg" (lambda (args) (bg (%job-spec->job (first args)))))

(defun %require-job (spec)
  (or (%job-spec->job spec)
      (error 'shell-parse-error :line (format nil "no such job: ~A" (or spec "")))))

(defun %job-ref-p (s) (and (stringp s) (plusp (length s)) (char= (char s 0) #\%)))

(defparameter +signal-names+
  `(("TERM" . ,+sigterm+) ("KILL" . ,+sigkill+) ("INT" . ,+sigint+) ("HUP" . 1)
    ("STOP" . ,+sigstop+) ("CONT" . ,+sigcont+) ("TSTP" . ,+sigtstp+)
    ("QUIT" . 3) ("USR1" . ,(if (member :darwin *features*) 30 10)))
  "Signal name -> number for the kill builtin.")

(defun %parse-signal (spec)
  "SPEC is a signal name or number without the leading dash: \"9\", \"KILL\",
\"SIGKILL\"."
  (let ((s (string-upcase spec)))
    (when (and (>= (length s) 3) (string= (subseq s 0 3) "SIG")) (setf s (subseq s 3)))
    (or (parse-integer s :junk-allowed t)
        (cdr (assoc s +signal-names+ :test #'string=))
        (error 'shell-parse-error :line (format nil "kill: unknown signal: ~A" spec)))))

(defun %parse-kill-args (args)
  "Split ARGS into (values SIGNAL TARGETS): a leading -SIG (name or number) sets
the signal, defaulting to SIGTERM."
  (if (and args (> (length (first args)) 1) (char= (char (first args) 0) #\-))
      (values (%parse-signal (subseq (first args) 1)) (rest args))
      (values +sigterm+ args)))

(define-builtin "kill"
  ;; kill [-SIGNAL] (%job | pid)...  A %job is terminated through the job (its
  ;; whole process group + threads reclaimed); a bare pid is signalled directly.
  (lambda (args)
    (multiple-value-bind (signal targets) (%parse-kill-args args)
      (unless targets (error 'shell-parse-error :line "kill: no target"))
      ;; Signal every target we can; collect the malformed ones and report them
      ;; once at the end, so one bad pid does not leave the rest un-signalled.
      (let ((bad '()))
        (dolist (target targets)
          (if (%job-ref-p target)
              (kill-job (%require-job target))
              (let ((pid (parse-integer target :junk-allowed t)))
                (if pid
                    (ignore-errors (c-kill pid signal))
                    (push target bad)))))
        (when bad
          (error 'shell-parse-error
                 :line (format nil "kill: illegal pid: ~{~A~^ ~}" (nreverse bad)))))
      (values))))

(define-builtin "wait"
  ;; wait [%job] — wait for one job (returning its output) or, with no argument,
  ;; every job.
  (lambda (args)
    (if args
        (wait-job (%require-job (first args)))
        (progn (dolist (j (all-jobs)) (ignore-errors (wait-job j))) (values)))))

(define-builtin "history"
  (lambda (args) (declare (ignore args))
    (loop for i below (history-count) collect (cons i (history-form i)))))

;;; --- directory stack: pushd / popd / dirs --------------------------------

(defvar *dir-stack* '()
  "Directory stack for pushd/popd, most-recently-pushed first; excludes the
current directory (which is always the stack's implicit top).")

(defun %dirs-list ()
  "The directory stack for display: current directory first, then pushed dirs."
  (cons (namestring *current-directory*) (mapcar #'namestring *dir-stack*)))

(define-builtin "pushd"
  (lambda (args)
    (let ((arg (first args)))
      (if (null arg)
          ;; no argument: swap the current directory with the top of the stack
          (if *dir-stack*
              (let ((cur *current-directory*))
                (%cd-to (namestring (pop *dir-stack*)))
                (push cur *dir-stack*))
              (error 'shell-parse-error :line "pushd: directory stack empty"))
          (progn (push *current-directory* *dir-stack*)
                 (%cd-to arg))))
    (%dirs-list)))

(define-builtin "popd"
  (lambda (args) (declare (ignore args))
    (if *dir-stack*
        (progn (%cd-to (namestring (pop *dir-stack*))) (%dirs-list))
        (error 'shell-parse-error :line "popd: directory stack empty"))))

(define-builtin "dirs" (lambda (args) (declare (ignore args)) (%dirs-list)))

;;; --- discoverability: help / type / which --------------------------------

(defparameter *builtin-docs*
  '(("cd"      . "cd [DIR]         change directory (handles ~, -, CDPATH; no arg = home)")
    ("pwd"     . "pwd              print the current directory")
    ("pushd"   . "pushd [DIR]      cd to DIR, pushing the current dir onto the stack")
    ("popd"    . "popd             cd to the directory popped off the stack")
    ("dirs"    . "dirs             list the directory stack")
    ("export"  . "export N=V ...   set environment variables")
    ("unset"   . "unset NAME ...   remove environment variables")
    ("alias"   . "alias [N=V ...]  define or list aliases")
    ("unalias" . "unalias N ...    remove aliases")
    ("jobs"    . "jobs             list background/stopped jobs")
    ("fg"      . "fg [%JOB]        resume a job in the foreground")
    ("bg"      . "bg [%JOB]        resume a job in the background")
    ("kill"    . "kill [-SIG] JOB  signal a job")
    ("wait"    . "wait [JOB]       wait for a job (or all jobs) to finish")
    ("history" . "history          list past command forms")
    ("source"  . "source FILE      run each line of FILE as shell input (also `.`)")
    ("type"    . "type NAME ...    report how each NAME resolves")
    ("which"   . "which NAME ...   print the path of each external NAME")
    ("help"    . "help [NAME ...]  list builtins, or describe the named ones")
    ("exit"    . "exit [N]         leave the shell"))
  "One-line help for each builtin, shown by the `help` builtin.")

(define-builtin "help"
  (lambda (args)
    (if args
        (loop for name in args
              collect (or (cdr (assoc name *builtin-docs* :test #'string=))
                          (format nil "~A: not a builtin" name)))
        (cons "consh builtins (help NAME for one):"
              (sort (loop for k being the hash-keys of *builtins*
                          collect (or (cdr (assoc k *builtin-docs* :test #'string=)) k))
                    #'string<)))))

(defun %classify-command (name)
  "A human-readable line describing how NAME resolves at the prompt."
  (cond
    ((builtin-p name) (format nil "~A is a shell builtin" name))
    ((gethash name *aliases*)
     (format nil "~A is aliased to `~A'" name (gethash name *aliases*)))
    ((nth-value 1 (gethash name *wrappers*))
     (format nil "~A is a wrapped command~@[ (~A)~]" name (%find-on-path name)))
    (t (let ((path (%find-on-path name)))
         (if path (format nil "~A is ~A" name path)
             (format nil "~A: not found" name))))))

(define-builtin "type" (lambda (args) (mapcar #'%classify-command args)))

(define-builtin "which"
  (lambda (args)
    (loop for name in args
          collect (or (%find-on-path name) (format nil "~A: not found" name)))))

;;; --- source: run a script of shell lines ---------------------------------

(define-builtin "source"
  (lambda (args)
    (let ((file (and args (probe-file (merge-pathnames (%expand-tilde (first args))
                                                       *current-directory*)))))
      (unless file
        (error 'shell-parse-error :line
               (format nil "source: no such file: ~A" (or (first args) ""))))
      ;; %eval-script-lines skips shebang/comment/blank lines and handles
      ;; multi-line forms; args after the file become $1.. within the script
      (let ((*script-args* (rest args))
            (*script-name* (namestring file)))
        (with-open-file (s file)
          (%eval-script-lines (lambda () (read-line s nil nil)))))
      (values))))

(define-builtin "." (builtin "source"))
