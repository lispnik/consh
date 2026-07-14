;;;; surface.lisp — the surface syntax layer (SPEC §7, §1).
;;;;
;;;; Reader-level sugar over real s-expressions: a bare line reads as a command
;;;; form, `,expr` and `$(expr)` escape to full Lisp, `|` builds a pipeline, `&`
;;;; backgrounds it.  Interactive and scripted use are one language because the
;;;; surface just DESUGARS to the pipeline/job forms of Phases 4–5:
;;;;
;;;;   ls -l /tmp            => (%shell-run (list (external "ls" "-l" "/tmp")))
;;;;   find / | grep foo     => (%shell-run (list (external "find" "/")
;;;;                                               (external "grep" "foo")))
;;;;   grep ,*pat* f &       => (%shell-run (list (external "grep" *pat* "f"))
;;;;                                        :background t)
;;;;
;;;; Also here: the prompt function, the (form . result) history, aliases, and
;;;; the completion generic (SPEC §1: the user environment is the image).

(in-package #:consh)

;;; ---------------------------------------------------------------------------
;;; Tokenizer
;;; ---------------------------------------------------------------------------

(define-condition shell-parse-error (error)
  ((line :initarg :line :reader shell-parse-error-line))
  (:report (lambda (c s) (format s "cannot parse command line ~S"
                                 (shell-parse-error-line c)))))

(defun %read-quoted (stream quote)
  "Read a quoted run, QUOTE already peeked.  Returns the inner string."
  (read-char stream)                    ; consume opening quote
  (with-output-to-string (out)
    (loop for c = (read-char stream nil nil)
          until (or (null c) (char= c quote))
          do (write-char c out))))

(defparameter +word-delimiters+ '(#\Space #\Tab #\Newline #\| #\& #\< #\>))

(defun %read-word (stream)
  "Read a bare word up to a delimiter (whitespace, | & < >).  Embedded quotes are
spliced in literally."
  (with-output-to-string (out)
    (loop for c = (peek-char nil stream nil nil)
          until (or (null c) (member c +word-delimiters+))
          do (if (member c '(#\" #\'))
                 (write-string (%read-quoted stream c) out)
                 (write-char (read-char stream) out)))))

(defun %fd-redirect-follows-p (stream)
  "STREAM is positioned at a digit; true iff the char right after it is < or >
\(so `2>` is a redirect but `2 >` / `2foo` is not).  Leaves the stream unmoved."
  (let ((d (read-char stream)))
    (prog1 (member (peek-char nil stream nil nil) '(#\< #\>))
      (unread-char d stream))))

(defun %read-redirect (stream fd)
  "At a `<` or `>` (FD is the explicit fd char or NIL), read the operator and
push the right redirection token kind.  Returns the token."
  (let ((c (read-char stream)))               ; consume < or >
    (if (char= c #\<)
        '(:redir . :in)
        (if (eql (peek-char nil stream nil nil) #\>)
            (progn (read-char stream)
                   (cons :redir (if (eql fd #\2) :err-append :out-append)))
            (cons :redir (if (eql fd #\2) :err :out))))))

(defun tokenize (line)
  "Tokenize LINE into a list of tokens, each one of:
  (:word . string)     a literal word (quotes stripped),
  (:escape . form)     a Lisp form from `,form` or `$(form)`,
  (:pipe)              a | separator,
  (:amp)               a trailing & (background),
  (:redir . KIND)      a redirection operator (:in :out :out-append :err
                       :err-append); the next :word token is its target."
  (with-input-from-string (s line)
    (let ((tokens '()))
      (loop for c = (peek-char nil s nil nil)
            while c
            do (cond
                 ((member c '(#\Space #\Tab #\Newline)) (read-char s))
                 ((char= c #\|) (read-char s) (push '(:pipe) tokens))
                 ((char= c #\&) (read-char s) (push '(:amp) tokens))
                 ((or (char= c #\<) (char= c #\>)) (push (%read-redirect s nil) tokens))
                 ((char= c #\,) (read-char s) (push (cons :escape (read s nil nil)) tokens))
                 ((char= c #\$)
                  (read-char s)
                  (if (eql (peek-char nil s nil nil) #\()
                      (push (cons :escape (read s nil nil)) tokens)          ; $(form)
                      (push (cons :word (concatenate 'string "$" (%read-word s)))
                            tokens)))                                        ; $VAR / ${VAR}
                 ((member c '(#\" #\')) (push (cons :word (%read-quoted s c)) tokens))
                 ;; N>  /  N>>  — a digit fd immediately before a redirect
                 ((and (member c '(#\1 #\2)) (%fd-redirect-follows-p s))
                  (let ((fd (read-char s))) (push (%read-redirect s fd) tokens)))
                 (t (push (cons :word (%read-word s)) tokens))))
      (nreverse tokens))))

;;; ---------------------------------------------------------------------------
;;; Aliases (SPEC §1: aliases are functions/expansions of the image)
;;; ---------------------------------------------------------------------------

(defvar *aliases* (make-hash-table :test 'equal)
  "Command name -> expansion string.")

(defun define-alias (name expansion)
  "Alias NAME to the command line EXPANSION (a string)."
  (setf (gethash name *aliases*) expansion))

(defun remove-alias (name) (remhash name *aliases*))

(defun %expand-alias (stage-tokens)
  "If STAGE-TOKENS begins with an aliased word, splice its expansion in front."
  (let ((head (first stage-tokens)))
    (if (and head (eq (car head) :word) (gethash (cdr head) *aliases*))
        (append (tokenize (gethash (cdr head) *aliases*)) (rest stage-tokens))
        stage-tokens)))

;;; ---------------------------------------------------------------------------
;;; $VAR / ${VAR} environment expansion
;;; ---------------------------------------------------------------------------

(defun %var-name-char-p (c &optional firstp)
  (and c (or (alpha-char-p c) (char= c #\_) (and (not firstp) (digit-char-p c)))))

(defun %var-ref-at (string i)
  "If STRING has a $-var reference whose name starts at index I (the char just
after `$`), return (values name index-after-ref), else (values NIL I)."
  (let ((n (length string)))
    (cond
      ((and (< i n) (char= (char string i) #\{))
       (let ((close (position #\} string :start (1+ i))))
         (if close (values (subseq string (1+ i) close) (1+ close)) (values nil i))))
      ((and (< i n) (%var-name-char-p (char string i) t))
       (let ((end (or (position-if-not #'%var-name-char-p string :start i) n)))
         (values (subseq string i end) end)))
      (t (values nil i)))))

(defun %expand-vars (string)
  "Replace $NAME and ${NAME} in STRING with the environment value (empty if
unset).  A `$` not starting a valid reference stays literal."
  (if (find #\$ string)
      (with-output-to-string (out)
        (let ((i 0) (n (length string)))
          (loop while (< i n) do
            (if (char= (char string i) #\$)
                (multiple-value-bind (name next) (%var-ref-at string (1+ i))
                  (if name
                      (progn (write-string (or (sb-ext:posix-getenv name) "") out)
                             (setf i next))
                      (progn (write-char #\$ out) (incf i))))
                (progn (write-char (char string i) out) (incf i))))))
      string))

;;; ---------------------------------------------------------------------------
;;; Globbing (SPEC §1: a function returning pathname objects)
;;; ---------------------------------------------------------------------------

(defun %match-set (pattern i ch)
  "PATTERN[I] is `[`.  Match CH against the set, returning (values matched-p
index-after-])."
  (let ((j (1+ i)) (n (length pattern)) (negate nil) (matched nil))
    (when (and (< j n) (member (char pattern j) '(#\! #\^))) (setf negate t) (incf j))
    (loop while (and (< j n) (char/= (char pattern j) #\])) do
      (if (and (< (+ j 2) n) (char= (char pattern (1+ j)) #\-)
               (char/= (char pattern (+ j 2)) #\]))
          (progn (when (char<= (char pattern j) ch (char pattern (+ j 2)))
                   (setf matched t))
                 (incf j 3))
          (progn (when (char= (char pattern j) ch) (setf matched t))
                 (incf j))))
    (values (if negate (not matched) matched) (1+ j))))   ; skip the closing ]

(defun %glob-match-p (pattern name)
  "True if shell glob PATTERN (`*` any run, `?` one char, `[set]`) matches NAME.
A leading dot in NAME is not matched by a leading `*`/`?` (Unix convention)."
  (when (and (plusp (length name)) (char= (char name 0) #\.)
             (plusp (length pattern)) (not (char= (char pattern 0) #\.)))
    (return-from %glob-match-p nil))
  (labels ((m (px sx)
             (cond
               ((= px (length pattern)) (= sx (length name)))
               ((char= (char pattern px) #\*)
                (or (m (1+ px) sx)
                    (and (< sx (length name)) (m px (1+ sx)))))
               ((= sx (length name)) nil)
               ((char= (char pattern px) #\?) (m (1+ px) (1+ sx)))
               ((char= (char pattern px) #\[)
                (multiple-value-bind (ok next) (%match-set pattern px (char name sx))
                  (and ok (m next (1+ sx)))))
               ((char= (char pattern px) (char name sx)) (m (1+ px) (1+ sx)))
               (t nil))))
    (m 0 0)))

(defun %glob-chars-p (string)
  (find-if (lambda (c) (member c '(#\* #\? #\[))) string))

(defun %basename (pathname)
  (if (and (null (pathname-name pathname)) (pathname-directory pathname))
      (car (last (pathname-directory pathname)))     ; a directory: its name, no /
      (file-namestring pathname)))

(defun glob (pattern &key (directory *current-directory*))
  "Return the pathnames under DIRECTORY matching shell PATTERN (`*` `?` `[set]`),
sorted.  A pattern with a directory part globs its basename in that
subdirectory.  Resolves against *current-directory*, never the process cwd."
  (let* ((slash (position #\/ pattern :from-end t))
         (subdir (if slash (subseq pattern 0 (1+ slash)) ""))
         (base-pat (if slash (subseq pattern (1+ slash)) pattern))
         (base (merge-pathnames subdir directory))
         (entries (ignore-errors
                   (directory (merge-pathnames (make-pathname :name :wild :type :wild)
                                               base)))))
    (sort (loop for p in entries
                when (%glob-match-p base-pat (%basename p))
                  collect (merge-pathnames (concatenate 'string subdir (%basename p))
                                           directory))
          #'string< :key #'namestring)))

;;; ---------------------------------------------------------------------------
;;; Parsing a command line into a Lisp form
;;; ---------------------------------------------------------------------------

(defun %expand-word-arg (word)
  "Expand a bare word into arg strings: $VAR first, then glob.  A glob that
matches nothing stays literal (bash default)."
  (let ((expanded (%expand-vars word)))
    (if (%glob-chars-p expanded)
        (let ((matches (glob expanded)))
          (if matches (mapcar #'namestring matches) (list expanded)))
        (list expanded))))

(defun %expand-stage-args (tokens)
  "Expand vars + globs across TOKENS into a flat list of argument forms (strings
and, for escapes, Lisp forms)."
  (loop for tok in tokens
        append (ecase (car tok)
                 (:word (%expand-word-arg (cdr tok)))
                 (:escape (list (cdr tok))))))

(defun %split-redirects (tokens)
  "Return (values argument-tokens redirections-alist).  Each (:redir . KIND)
token consumes the following :word as its (var-expanded) target."
  (let ((args '()) (redirs '()) (rest tokens))
    (loop while rest do
      (let ((tok (pop rest)))
        (if (eq (car tok) :redir)
            (let ((target (pop rest)))
              (unless (and target (eq (car target) :word))
                (error 'shell-parse-error :line "redirection is missing a target"))
              (push (cons (cdr tok) (%expand-vars (cdr target))) redirs))
            (push tok args))))
    (values (nreverse args) (nreverse redirs))))

(defun %split-pipe (tokens)
  "Split TOKENS on (:pipe) into a list of per-stage token lists."
  (let ((stages '()) (current '()))
    (dolist (tok tokens)
      (if (eq (car tok) :pipe)
          (progn (push (nreverse current) stages) (setf current '()))
          (push tok current)))
    (push (nreverse current) stages)
    (nreverse stages)))

(defun %stage->form (stage-tokens)
  (let ((tokens (%expand-alias stage-tokens)))
    (unless tokens (error 'shell-parse-error :line "empty pipeline stage"))
    (multiple-value-bind (arg-tokens redirs) (%split-redirects tokens)
      (let ((args (%expand-stage-args arg-tokens)))
        (if redirs
            `(external ,@args :redirections ',redirs)
            (cons 'external args))))))

(defun parse-shell-line (line)
  "Desugar a command LINE into a Lisp form that runs it.  NIL for a blank line.
A single-stage foreground command whose name is a builtin desugars to a builtin
call; everything else desugars to a pipeline run."
  (let ((tokens (tokenize line)))
    (when (null tokens) (return-from parse-shell-line nil))
    (let* ((background (eq (caar (last tokens)) :amp))
           (tokens (if background (butlast tokens) tokens))
           (stages (%split-pipe tokens)))
      ;; single-stage foreground builtin?
      (when (and (= 1 (length stages)) (not background))
        (let* ((toks (%expand-alias (first stages)))
               (head (first toks))
               (name (and head (eq (car head) :word) (%expand-vars (cdr head)))))
          (when (and name (builtin-p name))
            (multiple-value-bind (arg-toks redirs) (%split-redirects (rest toks))
              (declare (ignore redirs))          ; builtins ignore redirections
              (return-from parse-shell-line
                (list '%builtin name (cons 'list (%expand-stage-args arg-toks))))))))
      (list '%shell-run
            (cons 'list (mapcar #'%stage->form stages))
            :background background))))

(defun %run-foreground (pipeline)
  "Run PIPELINE in the foreground and return its collected output.  On a real
tty, hand the pipeline's process group the controlling terminal while it runs —
so keyboard signals (C-c) reach the running command, not the shell, and a command
reading the tty works — then reclaim the terminal.  A no-op handoff without a tty
or for a pure-Lisp pipeline (no process group)."
  (if (not (terminal-job-control-active-p))
      (pipeline-collect pipeline)
      (let ((result (run-pipeline pipeline)))
        (give-terminal-to-pgid (run-state-pgid (pipeline-result-state result)))
        (unwind-protect (pipeline-collect result)
          (reclaim-terminal)))))

(defun %shell-run (stages &key background)
  "Run the desugared STAGES: collect output (foreground) or start a job (&)."
  (let ((pipeline (make-pipeline stages)))
    (if background
        (run-job pipeline :background t)
        (%run-foreground pipeline))))

;;; ---------------------------------------------------------------------------
;;; Builtins (SPEC §1: the user environment is the image)
;;; ---------------------------------------------------------------------------

(define-condition shell-exit (error)
  ((code :initarg :code :initform 0 :reader shell-exit-code))
  (:report (lambda (c s) (format s "exit ~D" (shell-exit-code c)))))

(defvar *builtins* (make-hash-table :test 'equal)
  "Command name -> function of one argument (the list of arg strings).")
(defvar *previous-directory* nil "The directory `cd -` returns to.")

(defun define-builtin (name fn) (setf (gethash name *builtins*) fn))
(defun builtin (name) (values (gethash name *builtins*)))
(defun builtin-p (name) (nth-value 1 (gethash name *builtins*)))
(defun %builtin (name args) (funcall (builtin name) args))

(defun %ensure-directory-pathname (namestring)
  (if (and (plusp (length namestring))
           (char= (char namestring (1- (length namestring))) #\/))
      namestring
      (concatenate 'string namestring "/")))

(defun %job-spec->job (spec)
  "Resolve a job spec (\"%2\", \"2\", or NIL for the most recent) to a job."
  (if (null spec)
      (car (last (all-jobs)))
      (let ((id (parse-integer (string-left-trim "%" spec) :junk-allowed t)))
        (and id (find-job id)))))

(define-builtin "cd"
  (lambda (args)
    (let ((dir (cond ((null args) (namestring (user-homedir-pathname)))
                     ((and (string= (first args) "-") *previous-directory*)
                      (namestring *previous-directory*))
                     (t (first args)))))
      (let ((new (handler-case
                     (truename (merge-pathnames (%ensure-directory-pathname dir)
                                                *current-directory*))
                   (file-error () (error 'shell-parse-error :line
                                         (format nil "cd: no such directory: ~A" dir))))))
        (setf *previous-directory* *current-directory*
              *current-directory* new)
        (namestring new)))))

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

;;; ---------------------------------------------------------------------------
;;; History (SPEC §1: a sequence of (form . result), results hold live objects)
;;; ---------------------------------------------------------------------------

(defvar *history* (make-array 0 :adjustable t :fill-pointer 0)
  "Vector of (FORM . RESULT) pairs; RESULT holds the live objects produced.")

(defun record-history (form result)
  (vector-push-extend (cons form result) *history*)
  result)

(defun history-count () (fill-pointer *history*))

(defun history-ref (n)
  "The Nth (FORM . RESULT) pair (0-based)."
  (aref *history* n))

(defun history-form (n) (car (history-ref n)))
(defun history-result (n) (cdr (history-ref n)))

(defun last-result ()
  "The most recent result, or NIL if history is empty."
  (if (plusp (history-count))
      (cdr (aref *history* (1- (history-count))))
      nil))

(defun clear-history () (setf (fill-pointer *history*) 0))

;;; ---------------------------------------------------------------------------
;;; Evaluation: bare line -> command, `(` line -> full Lisp (one language)
;;; ---------------------------------------------------------------------------

(defun %lisp-line-p (line)
  "A line beginning with ( or ` is a full Lisp form, not a command."
  (and (plusp (length line)) (member (char line 0) '(#\( #\`))))

(defun shell-eval (line &key (record t))
  "Evaluate a surface LINE: a Lisp form if it starts with `(`, otherwise a
desugared command.  Records (form . result) in *history*.  Returns
(values result form)."
  (let* ((trimmed (string-trim '(#\Space #\Tab #\Newline) line))
         (form (cond ((zerop (length trimmed)) nil)
                     ((%lisp-line-p trimmed) (read-from-string trimmed))
                     (t (parse-shell-line trimmed)))))
    (if (null form)
        (values nil nil)
        (let ((result (eval form)))
          (when record (record-history form result))
          (values result form)))))

;;; ---------------------------------------------------------------------------
;;; User init file (SPEC §1: the environment is the image — configure it in Lisp)
;;; ---------------------------------------------------------------------------
;;;
;;; Loaded once at REPL startup: plain Lisp evaluated in the CONSH package, so it
;;; can define aliases, set *prompt-function*, register wrappers, define builtins.
;;; Location: $XDG_CONFIG_HOME/consh/consh.lisp, else ~/.config/consh/consh.lisp.

(defvar *load-init-file* t
  "When NIL, the REPL skips loading the user init file.")

(defun %config-home ()
  "The XDG config directory: $XDG_CONFIG_HOME if set, else ~/.config/."
  (let ((xdg (sb-ext:posix-getenv "XDG_CONFIG_HOME")))
    (if (and xdg (plusp (length xdg)))
        (pathname (%ensure-directory-pathname xdg))
        (merge-pathnames ".config/" (user-homedir-pathname)))))

(defun init-file-path ()
  "Pathname of the user init file (whether or not it exists)."
  (merge-pathnames "consh/consh.lisp" (%config-home)))

(defun load-init-file (&key (path (init-file-path)))
  "Load the user init file at PATH (in the CONSH package) if it exists.  Any
error is reported and swallowed — a broken init file must never stop the shell
from starting.  Returns the truename loaded, or NIL if there was no file."
  (let ((file (probe-file path)))
    (when file
      (handler-case
          (let ((*package* (find-package '#:consh)))
            (load file)
            file)
        (serious-condition (e)
          (format *error-output* "~&consh: error loading ~A:~%  ~A~%" file e)
          nil)))))

;;; ---------------------------------------------------------------------------
;;; Prompt (SPEC §1: the prompt is a function)
;;; ---------------------------------------------------------------------------

(defun default-prompt ()
  (let* ((dir (pathname-directory *current-directory*))
         (name (if (and (consp dir) (cdr dir)) (car (last dir)) "/")))
    (format nil "consh ~A> " name)))

(defvar *prompt-function* #'default-prompt
  "A function of no arguments returning the prompt string.")

(defun prompt () (funcall *prompt-function*))

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

(defun %split-string (string char)
  (loop with start = 0
        for pos = (position char string :start start)
        collect (subseq string start pos)
        while pos do (setf start (1+ pos))))

(defun %as-directory (namestring)
  (if (and (plusp (length namestring))
           (char= (char namestring (1- (length namestring))) #\/))
      namestring
      (concatenate 'string namestring "/")))

(defmethod complete ((kind (eql :command)) text &key)
  (let ((names '()))
    (maphash (lambda (k v) (declare (ignore v))
               (when (%prefixp text k) (push k names)))
             *wrappers*)
    (maphash (lambda (k v) (declare (ignore v))       ; builtins
               (when (%prefixp text k) (push k names)))
             *builtins*)
    (sort (remove-duplicates (append names (ignore-errors (%path-commands text)))
                             :test #'string=)
          #'string<)))

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
         (base (merge-pathnames subdir directory))
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
    (cond ((null last-space) (complete :command token))
          ((and (plusp (length token)) (member (char token 0) '(#\, #\()))
           (complete :symbol (string-left-trim ",(" token)))
          (t (complete :path token)))))

;;; The REPL (shell-repl / main) lives in lineedit.lisp, which loads after this
;;; file and adds the interactive line editor it uses.
