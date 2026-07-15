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

(define-condition shell-input-incomplete (shell-parse-error) ()
  (:documentation
   "A line is a syntactically incomplete PREFIX that more input could finish — an
unterminated quote or an unbalanced Lisp form.  It is a subtype of
SHELL-PARSE-ERROR, so batch use still reports it as a parse error; the
interactive REPL instead reads a continuation line.")
  (:report (lambda (c s) (format s "incomplete input: ~S"
                                 (shell-parse-error-line c)))))

(defun %read-quoted (stream quote)
  "Read a quoted run, QUOTE already peeked.  Returns the inner string.  Signals
SHELL-PARSE-ERROR on an unterminated quote (EOF before the closing QUOTE) rather
than silently accepting the rest of the line."
  (read-char stream)                    ; consume opening quote
  (let ((out (make-string-output-stream)))
    (loop for c = (read-char stream nil nil) do
      (cond ((null c) (error 'shell-input-incomplete
                             :line (format nil "unterminated ~C quote" quote)))
            ((char= c quote) (return (get-output-stream-string out)))
            (t (write-char c out))))))

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
  "At a `<` or `>` (FD is the explicit fd char, e.g. #\\2, or NIL for the default),
read the operator and return the redirection token:
  (:redir . KIND)         a file redirect (next :word is its target),
  (:redir-dup FROM TO)    an fd duplication like `2>&1` / `>&2` (self-contained),
  (:redir-both . KIND)    `>&word` = both stdout+stderr to the next :word."
  (let ((c (read-char stream)))               ; consume < or >
    (cond
      ((char= c #\<) '(:redir . :in))
      ;; `>>` append
      ((eql (peek-char nil stream nil nil) #\>)
       (read-char stream)
       (cons :redir (if (eql fd #\2) :err-append :out-append)))
      ;; `>&` — fd duplication (`>&2`, `2>&1`) or `>&word` (both to a file)
      ((eql (peek-char nil stream nil nil) #\&)
       (read-char stream)                     ; consume &
       (let ((nxt (peek-char nil stream nil nil)))
         (if (and nxt (digit-char-p nxt))
             (progn (read-char stream)
                    (list :redir-dup
                          (if (eql fd #\2) 2 1)                 ; FROM fd
                          (- (char-code nxt) (char-code #\0)))) ; TO fd
             (cons :redir-both :trunc))))      ; >&word -> both to word
      (t (cons :redir (if (eql fd #\2) :err :out))))))

(defun %read-escape-form (stream line)
  "Read one Lisp form for a `,` or `$(...)` escape.  A malformed form (unbalanced
parens, a reader error) becomes a clean SHELL-PARSE-ERROR rather than a raw
END-OF-FILE / READER-ERROR leaking out of the tokenizer."
  (handler-case (read stream nil nil)
    (serious-condition ()
      (error 'shell-parse-error :line line))))

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
                 ((char= c #\&)
                  (read-char s)
                  (if (eql (peek-char nil s nil nil) #\>)
                      ;; `&>word` / `&>>word` — both stdout+stderr to the word
                      (progn (read-char s)              ; consume >
                             (let ((appendp (eql (peek-char nil s nil nil) #\>)))
                               (when appendp (read-char s))
                               (push (cons :redir-both (if appendp :append :trunc)) tokens)))
                      (push '(:amp) tokens)))            ; plain trailing & (background)
                 ((or (char= c #\<) (char= c #\>)) (push (%read-redirect s nil) tokens))
                 ((char= c #\,) (read-char s)
                  (push (cons :escape (%read-escape-form s line)) tokens))
                 ((char= c #\$)
                  (read-char s)
                  (if (eql (peek-char nil s nil nil) #\()
                      (push (cons :escape (%read-escape-form s line)) tokens) ; $(form)
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
;;; $VAR / ${VAR} expansion, script positional parameters ($0 $1 $@ $# $* $?)
;;; ---------------------------------------------------------------------------

(defvar *script-args* nil
  "Positional parameters of the running script: a list of argument strings, so
$1 is (first *script-args*).  NIL outside a script.")

(defvar *script-name* nil
  "Name of the running script ($0), or NIL outside a script (then $0 is \"consh\").")

(defun %positional-arg (k)
  "The K-th positional parameter as a string: $0 is the script name, $1.. are
*script-args* (1-based); out of range is the empty string."
  (cond ((zerop k) (or *script-name* "consh"))
        ((<= k (length *script-args*)) (nth (1- k) *script-args*))
        (t "")))

(defun %special-var (ch)
  "The value of a special parameter: #=arg count, @/*=all args joined, ?=status."
  (case ch
    (#\# (princ-to-string (length *script-args*)))
    ((#\@ #\*) (join-with-space *script-args*))
    (#\? (princ-to-string *last-status*))
    (t "")))

(defun script-arg (n) (%positional-arg n))
(defun script-args () *script-args*)

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

(defun %resolve-var (name)
  "Resolve a ${...}-style reference NAME: all-digits is a positional parameter, a
lone # @ * ? is a special parameter, else an environment variable."
  (cond ((zerop (length name)) "")
        ((every #'digit-char-p name) (%positional-arg (parse-integer name)))
        ((and (= 1 (length name)) (member (char name 0) '(#\# #\@ #\* #\?)))
         (%special-var (char name 0)))
        (t (or (sb-ext:posix-getenv name) ""))))

(defun %expand-vars (string)
  "Replace $NAME / ${NAME} with an environment value, and the script parameters
$0 $1 … $# $@ $* $? with their values (empty when unset/out of range).  A bare
$<digits> is the whole number (use ${N} next to more digits).  A `$` not starting
a valid reference stays literal."
  (if (find #\$ string)
      (with-output-to-string (out)
        (let ((i 0) (n (length string)))
          (loop while (< i n) do
            (if (char= (char string i) #\$)
                (let ((nxt (and (< (1+ i) n) (char string (1+ i)))))
                  (cond
                    ;; $#  $@  $*  $?
                    ((and nxt (member nxt '(#\# #\@ #\* #\?)))
                     (write-string (%special-var nxt) out) (incf i 2))
                    ;; $<digits> — positional parameter
                    ((and nxt (digit-char-p nxt))
                     (let ((end (or (position-if-not #'digit-char-p string :start (1+ i)) n)))
                       (write-string (%positional-arg
                                      (parse-integer string :start (1+ i) :end end)) out)
                       (setf i end)))
                    ;; $NAME / ${NAME}
                    (t (multiple-value-bind (name next) (%var-ref-at string (1+ i))
                         (if name
                             (progn (write-string (%resolve-var name) out) (setf i next))
                             (progn (write-char #\$ out) (incf i)))))))
                (progn (write-char (char string i) out) (incf i))))))
      string))

;;; --- option parsing for scripts -------------------------------------------

(defun %spec-value-p (type)
  "True when a spec TYPE takes a value (string), false for a boolean flag.
Matched by name so `string`/`boolean` in any package (or the keywords) work."
  (let ((name (string type)))
    (cond ((string-equal name "STRING") t)
          ((string-equal name "BOOLEAN") nil)
          (t (error 'shell-parse-error :line (format nil "parse-args: bad option type ~S" type))))))

(defun %find-option (flag spec)
  "The spec entry (KEY FLAG... TYPE) whose flags include FLAG, or NIL."
  (find-if (lambda (entry) (member flag (butlast (rest entry)) :test #'string=)) spec))

(defun parse-args (args spec)
  "Parse ARGS (a list of strings) against SPEC and return (values OPTIONS-PLIST
POSITIONALS).  Each SPEC entry is (KEY FLAG... TYPE): FLAGs are option strings
like \"-v\"/\"--verbose\", TYPE is boolean or string.  Supports --opt=val,
--opt val, -o val, -o=val, and `--` to end option parsing.  A boolean option sets
its KEY to T; a missing value or an unknown option signals an error."
  (let ((opts '()) (positionals '()) (rest args) (end-of-opts nil))
    (loop while rest do
      (let ((arg (pop rest)))
        (cond
          (end-of-opts (push arg positionals))
          ((string= arg "--") (setf end-of-opts t))
          ((and (> (length arg) 1) (char= (char arg 0) #\-))
           (let* ((eq (position #\= arg))
                  (flag (if eq (subseq arg 0 eq) arg))
                  (inline-val (and eq (subseq arg (1+ eq))))
                  (entry (%find-option flag spec)))
             (unless entry
               (error 'shell-parse-error :line (format nil "parse-args: unknown option ~A" flag)))
             (let ((key (first entry)))
               (if (%spec-value-p (car (last entry)))
                   (let ((val (or inline-val
                                  (if rest (pop rest)
                                      (error 'shell-parse-error
                                             :line (format nil "parse-args: ~A needs a value" flag))))))
                     (setf (getf opts key) val))
                   (setf (getf opts key) t)))))
          (t (push arg positionals)))))
    (values opts (nreverse positionals))))

;;; ---------------------------------------------------------------------------
;;; Globbing (SPEC §1: a function returning pathname objects)
;;; ---------------------------------------------------------------------------

(defun %match-set (pattern i ch)
  "PATTERN[I] is `[`.  Match CH against the set, returning (values matched-p
index-after-] closed-p).  CLOSED-P is NIL when the `[` has no matching `]`, in
which case the caller should treat the `[` as a literal character."
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
    (if (< j n)                                           ; j is at the closing ]
        (values (if negate (not matched) matched) (1+ j) t)
        (values nil j nil))))                             ; unterminated set

(defun %match-one (pattern px ch)
  "Match the single (non-`*`) pattern element at PX against char CH.  Returns the
pattern index after the element on a match, else NIL.  Handles `?`, `[set]` (and
an unmatched `[` as a literal), and a literal character."
  (let ((pc (char pattern px)))
    (cond
      ((char= pc #\?) (1+ px))
      ((char= pc #\[)
       (multiple-value-bind (ok next closed) (%match-set pattern px ch)
         (cond ((not closed) (and (char= ch #\[) (1+ px)))   ; literal [
               (ok next)
               (t nil))))
      ((char= pc ch) (1+ px))
      (t nil))))

(defun %glob-match-p (pattern name)
  "True if shell glob PATTERN (`*` any run, `?` one char, `[set]`) matches NAME.
A leading dot in NAME is not matched by a leading `*`/`?` (Unix convention).
Uses the linear two-pointer algorithm with a single backtrack point per `*`, so
it cannot exhibit the exponential blow-up a naive recursive matcher does on
patterns like `**********b`."
  (when (and (plusp (length name)) (char= (char name 0) #\.)
             (plusp (length pattern)) (not (char= (char pattern 0) #\.)))
    (return-from %glob-match-p nil))
  (let ((plen (length pattern)) (nlen (length name))
        (px 0) (sx 0) (star-px -1) (star-sx 0))
    (loop
      (cond
        ((< sx nlen)
         (let ((next (and (< px plen) (char/= (char pattern px) #\*)
                          (%match-one pattern px (char name sx)))))
           (cond
             (next (setf px next sx (1+ sx)))                    ; element matched
             ((and (< px plen) (char= (char pattern px) #\*))    ; record `*`, skip it
              (setf star-px px star-sx sx px (1+ px)))
             ((>= star-px 0)                                     ; backtrack: `*` eats one more
              (setf px (1+ star-px) star-sx (1+ star-sx) sx star-sx))
             (t (return nil)))))
        (t                                                       ; name consumed
         (loop while (and (< px plen) (char= (char pattern px) #\*)) do (incf px))
         (return (= px plen)))))))

(defun %glob-chars-p (string)
  (find-if (lambda (c) (member c '(#\* #\? #\[))) string))

(defun %basename (pathname)
  (if (and (null (pathname-name pathname)) (pathname-directory pathname))
      (let ((last (car (last (pathname-directory pathname)))))
        (if (stringp last) last ""))                 ; a directory: its name (root -> "")
      (file-namestring pathname)))

(defun %dir-children (dir)
  "Pathnames of the entries (files and subdirectories) directly under the
directory pathname DIR; NIL if DIR cannot be listed."
  (ignore-errors
   (directory (merge-pathnames (make-pathname :name :wild :type :wild) dir))))

(defun %dir-pathname-p (p)
  "True if pathname P denotes a directory (no name/type, has directory parts)."
  (and (null (pathname-name p)) (null (pathname-type p)) (pathname-directory p)))

(defun %glob-walk (comps dirs need-dir-last)
  "Expand the remaining pattern COMPS (component strings) against the candidate
directory pathnames DIRS, returning the matching pathnames.  A component with
glob chars is matched against each directory's entries; a literal one is probed.
Any component that is not the last, and the last one when NEED-DIR-LAST (a
trailing `/`), keeps only directories — so we can descend and so `*/` yields only
directories."
  (if (null comps)
      dirs
      (let* ((comp (first comps)) (more (rest comps))
             (need-dir (or more need-dir-last))
             (out '()))
        (dolist (dir dirs)
          (if (%glob-chars-p comp)
              (dolist (child (%dir-children dir))
                (when (and (%glob-match-p comp (%basename child))
                           (or (not need-dir) (%dir-pathname-p child)))
                  (push child out)))
              (let ((child (probe-file (merge-pathnames comp dir))))
                (when (and child (or (not need-dir) (%dir-pathname-p child)))
                  (push child out)))))
        (%glob-walk more (nreverse out) need-dir-last))))

(defun glob (pattern &key (directory *current-directory*))
  "Return the pathnames matching shell PATTERN (`*` `?` `[set]`), sorted.  EACH
path component may be a glob — `*/`, `*/foo`, `src/*/x` all expand — walking the
filesystem component by component from DIRECTORY (or `/` for an absolute
pattern).  Resolves against *current-directory*, never the process cwd.  A
relative pattern yields RELATIVE pathnames (`a.txt`, `sub/x.txt`), like a shell,
so they resolve against a spawned child's chdir'd cwd; an absolute pattern yields
absolute pathnames.  A PATTERN that is not a valid pathname simply matches
nothing (the caller keeps the word literal) — never a raw pathname-parse error."
  (handler-case
      (let* ((n (length pattern))
             (absolutep (and (plusp n) (char= (char pattern 0) #\/)))
             (need-dir-last (and (plusp n) (char= (char pattern (1- n)) #\/)))
             (comps (remove "" (%split-string pattern #\/) :test #'string=))
             (root (if absolutep #p"/" (pathname directory)))
             (matches (%glob-walk comps (list root) need-dir-last)))
        (sort (if absolutep
                  matches
                  ;; make each match relative to the pattern's base directory
                  (mapcar (lambda (p) (pathname (enough-namestring p directory))) matches))
              #'string< :key #'namestring))
    (error () nil)))

;;; ---------------------------------------------------------------------------
;;; Parsing a command line into a Lisp form
;;; ---------------------------------------------------------------------------

(defun %expand-tilde (word)
  "Expand a leading ~ / ~user to a home directory: `~` or `~/x` to the current
user's home, `~name` / `~name/x` to NAME's home.  A word not starting with `~`,
or a `~name` for an unknown user, is returned unchanged (bash behaviour)."
  (if (and (plusp (length word)) (char= (char word 0) #\~))
      (let* ((slash (position #\/ word))
             (name  (subseq word 1 slash))
             (rest  (if slash (subseq word slash) ""))
             (home  (if (zerop (length name))
                        (namestring (user-homedir-pathname))
                        (ignore-errors (sb-posix:passwd-dir (sb-posix:getpwnam name))))))
        (if home
            (concatenate 'string (string-right-trim "/" home) rest)
            word))
      word))

(defun %expand-word-arg (word)
  "Expand a bare word into arg strings: leading ~ then $VAR then glob.  A glob
that matches nothing stays literal (bash default)."
  (let ((expanded (%expand-vars (%expand-tilde word))))
    (if (%glob-chars-p expanded)
        (let ((matches (glob expanded)))
          (if matches (mapcar #'namestring matches) (list expanded)))
        (list expanded))))

(defun %expand-stage-args (tokens)
  "Expand vars + globs across TOKENS into a flat list of argument forms (strings
and, for escapes, Lisp forms)."
  (loop for tok in tokens
        append (case (car tok)
                 (:word (%expand-word-arg (cdr tok)))
                 (:escape (list (cdr tok)))
                 ;; :pipe/:redir are consumed earlier; a leftover (e.g. a `&`
                 ;; that is not trailing) is malformed — a clean error, not a
                 ;; raw CASE-FAILURE.
                 (t (error 'shell-parse-error
                           :line (format nil "unexpected token: ~S" tok))))))

(defun %redir-target (rest)
  "Pop and var-expand the :word target that a file redirection requires; error if
the next token is not a word.  Returns (values path remaining-tokens)."
  (let ((target (first rest)))
    (unless (and target (eq (car target) :word))
      (error 'shell-parse-error :line "redirection is missing a target"))
    (values (%expand-vars (cdr target)) (rest rest))))

(defun %split-redirects (tokens)
  "Return (values argument-tokens redirection-specs).  Specs are kept in
command-line ORDER (fd duplication depends on it) — each is one of:
  (KIND . path)      a file redirect (KIND :in :out :out-append :err :err-append),
  (:dup FROM TO)     dup2 TO onto FROM (from `2>&1` / `>&2`)."
  (let ((args '()) (redirs '()) (rest tokens))
    (loop while rest do
      (let ((tok (pop rest)))
        (case (car tok)
          (:redir
           (multiple-value-bind (path more) (%redir-target rest)
             (setf rest more)
             (push (cons (cdr tok) path) redirs)))
          (:redir-both                        ; `&>f` / `>&f`: stdout->f, stderr->stdout
           (multiple-value-bind (path more) (%redir-target rest)
             (setf rest more)
             (push (cons (if (eq (cdr tok) :append) :out-append :out) path) redirs)
             (push (list :dup 2 1) redirs)))
          (:redir-dup                          ; `2>&1` / `>&2`
           (push (list :dup (second tok) (third tok)) redirs))
          (t (push tok args)))))
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

(defvar *auto-cd* t
  "When true, a bare line naming an existing directory (and nothing else) changes
to it, as if `cd DIR` — but only when the name is not also a builtin, wrapper, or
$PATH command.")

(defun %safe-probe (pathspec)
  "probe-file that never signals — PATHSPEC may be a wild or otherwise invalid
namestring (e.g. one containing `[`), which raw probe-file/truename would error
on."
  (ignore-errors (probe-file pathspec)))

;;; ---------------------------------------------------------------------------
;;; Shared path/string helpers (used by parsing, completion, highlighting)
;;; ---------------------------------------------------------------------------

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

(defun %find-on-path (name)
  "First executable path for NAME: if NAME contains `/`, resolve it directly;
otherwise search $PATH.  Returns a namestring or NIL."
  (if (find #\/ name)
      (let ((p (%safe-probe (merge-pathnames name *current-directory*))))
        (and p (namestring p)))
      (let ((path (sb-ext:posix-getenv "PATH")))
        (when path
          (loop for dir in (%split-string path #\:)
                thereis (and (plusp (length dir))
                             (let ((p (%safe-probe (merge-pathnames name (%as-directory dir)))))
                               (and p (namestring p)))))))))

(defun %directory-arg-p (word)
  "True when WORD names an existing directory relative to *current-directory*."
  (let ((p (%safe-probe (merge-pathnames (%ensure-directory-pathname word)
                                         *current-directory*))))
    (and p (%dir-pathname-p p))))

(defun parse-shell-line (line)
  "Desugar a command LINE into a Lisp form that runs it.  NIL for a blank line.
A single-stage foreground command whose name is a builtin desugars to a builtin
call; a bare existing-directory name auto-cds; everything else desugars to a
pipeline run."
  (let ((tokens (tokenize line)))
    (when (null tokens) (return-from parse-shell-line nil))
    (let* ((background (eq (caar (last tokens)) :amp))
           (tokens (if background (butlast tokens) tokens))
           (stages (%split-pipe tokens)))
      ;; single-stage foreground builtin, or auto-cd?
      (when (and (= 1 (length stages)) (not background))
        (let* ((toks (%expand-alias (first stages)))
               (head (first toks))
               (name (and head (eq (car head) :word) (%expand-vars (cdr head)))))
          (when (and name (builtin-p name))
            (multiple-value-bind (arg-toks redirs) (%split-redirects (rest toks))
              (declare (ignore redirs))          ; builtins ignore redirections
              (return-from parse-shell-line
                (list '%builtin name (cons 'list (%expand-stage-args arg-toks))))))
          ;; auto-cd: a lone non-glob token that resolves to a directory and is
          ;; not a command (a glob is left to normal expansion, not auto-cd)
          (when (and *auto-cd* head (eq (car head) :word) (null (rest toks))
                     (not (%glob-chars-p (cdr head))))
            (let ((words (%expand-word-arg (cdr head))))
              (when (and (= 1 (length words))
                         (%directory-arg-p (first words))
                         (not (builtin-p name))
                         (not (nth-value 1 (gethash name *wrappers*)))
                         (not (%find-on-path name)))
                (return-from parse-shell-line
                  (list '%builtin "cd" (list 'list (first words)))))))))
      (list '%shell-run
            (cons 'list (mapcar #'%stage->form stages))
            :background background))))

(defvar *last-status* 0
  "Exit status of the most recent foreground command (0 = success).  Set by
%RUN-FOREGROUND and the REPL; read by PROMPT-EXIT-STATUS.")

(defun %pipeline-exit-status (result)
  "The exit status of a finished RESULT: 0 when every external succeeded (per each
wrapper's PARSE-ERROR-OUTPUT, so grep's benign exit 1 stays 0), else the first
failing stage's status."
  (let ((failure (%first-failure result)))
    (if failure (command-failed-status failure) 0)))

(defun %run-foreground (pipeline)
  "Run PIPELINE in the foreground and return its collected output.  On a real
tty, hand the pipeline's process group the controlling terminal while it runs —
so keyboard signals (C-c) reach the running command, not the shell, and a command
reading the tty works — then reclaim the terminal.  A no-op handoff without a tty
or for a pure-Lisp pipeline (no process group).  Records the exit status in
*LAST-STATUS* for the prompt."
  (let ((result (run-pipeline pipeline))
        (interactive (terminal-job-control-active-p)))
    (when interactive
      (give-terminal-to-pgid (run-state-pgid (pipeline-result-state result))))
    (unwind-protect
         (prog1 (pipeline-collect result)
           (setf *last-status* (%pipeline-exit-status result)))
      (when interactive (reclaim-terminal)))))

(defun %shell-run (stages &key background)
  "Run the desugared STAGES: collect output (foreground) or start a job (&)."
  (let ((pipeline (make-pipeline stages)))
    (if background
        (run-job pipeline :background t)
        (%run-foreground pipeline))))

;;; ---------------------------------------------------------------------------
;;; Builtin machinery (definitions live in builtins.lisp)
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

(defun %read-lisp-line (trimmed)
  "Read exactly one Lisp form from a `(`-line.  Signals a clean SHELL-PARSE-ERROR
on an unbalanced form (else a raw END-OF-FILE leaks) OR on trailing text after
the form (which would otherwise be silently dropped — e.g. `(+ 1 2) rm -rf /`
running only the first form)."
  (handler-case
      (multiple-value-bind (form pos) (read-from-string trimmed)
        (let ((rest (string-trim '(#\Space #\Tab #\Newline) (subseq trimmed pos))))
          (unless (zerop (length rest))
            (error 'shell-parse-error :line trimmed))
          form))
    ;; an unbalanced form runs out of input: incomplete, not a hard error
    (end-of-file () (error 'shell-input-incomplete :line trimmed))
    (shell-parse-error (e) (error e))
    (serious-condition () (error 'shell-parse-error :line trimmed))))

(defun %parse-line (trimmed)
  "Parse a trimmed surface line into a Lisp form (NIL for blank), without
evaluating.  Signals SHELL-INPUT-INCOMPLETE for an incomplete prefix."
  (cond ((zerop (length trimmed)) nil)
        ((%lisp-line-p trimmed) (%read-lisp-line trimmed))
        (t (parse-shell-line trimmed))))

(defun input-complete-p (text)
  "NIL when TEXT is a syntactically incomplete prefix (unbalanced Lisp form or
unterminated quote) that more input could finish; T otherwise (including a
genuine parse error, which is 'complete enough' to report)."
  (let ((trimmed (string-trim '(#\Space #\Tab #\Newline) text)))
    (handler-case (progn (%parse-line trimmed) t)
      (shell-input-incomplete () nil)
      (error () t))))

(defun shell-eval (line &key (record t))
  "Evaluate a surface LINE: a Lisp form if it starts with `(`, otherwise a
desugared command.  Records (form . result) in *history*.  Returns
(values result form)."
  (let* ((trimmed (string-trim '(#\Space #\Tab #\Newline) line))
         (form (%parse-line trimmed)))
    (if (null form)
        (values nil nil)
        (let ((result (eval form)))
          (when record (record-history form result))
          (values result form)))))

(defun %blank-or-comment-p (line)
  "True for a blank line, or a comment: first non-blank char `#` (shell-style, so
a `#!` shebang is skipped) or `;` (Lisp-style, for scripts that are mostly Lisp)."
  (let ((tr (string-left-trim '(#\Space #\Tab) line)))
    (or (zerop (length tr)) (member (char tr 0) '(#\# #\;)))))

(defun %eval-script-lines (next-line)
  "Run a script whose lines come from NEXT-LINE (a thunk returning the next line
string or NIL at EOF).  Blank / `#`-comment lines are skipped when not mid-form;
multi-line Lisp forms accumulate via INPUT-COMPLETE-P.  Each statement resets
*LAST-STATUS* then evaluates; a SHELL-EXIT propagates to the caller; any other
error is reported and sets *LAST-STATUS* to 1, then execution continues.  Returns
the final *LAST-STATUS*."
  (let ((buffer nil))
    (flet ((run (text)
             (handler-case
                 (progn
                   (setf *last-status* 0)
                   (let ((result (shell-eval text :record nil)))
                     ;; a command line's result IS its output — present it to
                     ;; stdout as the REPL would; a Lisp form stays silent unless
                     ;; it prints itself
                     (unless (%lisp-line-p (string-left-trim '(#\Space #\Tab) text))
                       (present result))))
               (shell-exit (c) (error c))                 ; propagate to the script runner
               (error (e) (setf *last-status* 1)
                 (format *error-output* "~&consh: ~A~%" e)))))
      (loop for line = (funcall next-line) while line do
        (cond
          (buffer                                          ; accumulating a multi-line form
           (setf buffer (concatenate 'string buffer (string #\Newline) line))
           (when (input-complete-p buffer) (run buffer) (setf buffer nil)))
          ((%blank-or-comment-p line))                     ; skip
          ((input-complete-p line) (run line))
          (t (setf buffer line))))                         ; start a multi-line form
      (when buffer (run buffer))))                         ; trailing incomplete: eval (errors cleanly)
  *last-status*)

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
;;;
;;; The prompt is a function of no arguments returning a string; set
;;; *prompt-function* (e.g. from ~/.config/consh/consh.lisp) to a lambda composed
;;; from these building blocks.  Colours via COLORIZE are cursor-safe: the line
;;; editor measures the prompt's VISIBLE width, skipping ANSI escapes.

(defun prompt-cwd-base (&optional (directory *current-directory*))
  "The final component of DIRECTORY (like the default prompt), or \"/\" for root."
  (let ((dir (pathname-directory directory)))
    (if (and (consp dir) (cdr dir)) (car (last dir)) "/")))

(defun prompt-cwd (&optional (directory *current-directory*))
  "DIRECTORY as a namestring with the home directory abbreviated to `~`."
  (let ((path (string-right-trim "/" (namestring directory)))
        (home (string-right-trim "/" (namestring (user-homedir-pathname)))))
    (cond ((string= path "") "/")
          ((string= path home) "~")
          ((and (> (length path) (length home))
                (string= home (subseq path 0 (length home)))
                (char= (char path (length home)) #\/))
           (concatenate 'string "~" (subseq path (length home))))
          (t path))))

(let ((cached nil))
  (defun prompt-user ()
    "The current user's login name (via getpwuid), memoized — it never changes."
    (or cached (setf cached (or (ignore-errors (uid-username (sb-posix:getuid))) "?")))))

(defun prompt-host ()
  "The short hostname (domain stripped)."
  (let ((h (machine-instance)))
    (subseq h 0 (or (position #\. h) (length h)))))

(defun %find-git-head (directory)
  "Walk up from DIRECTORY to a `.git/HEAD`, returning its pathname or NIL."
  (let ((dir (ignore-errors (truename directory))))
    (loop while dir do
      (let ((head (merge-pathnames ".git/HEAD" dir)))
        (when (probe-file head) (return head)))
      (let ((up (ignore-errors (truename (merge-pathnames "../" dir)))))
        (setf dir (and up (not (equal up dir)) up))))))   ; stop at the filesystem root

(defun prompt-git-branch (&optional (directory *current-directory*))
  "The current git branch (read from `.git/HEAD`, no subprocess), a short SHA when
detached, or NIL outside a repository."
  (let ((head (%find-git-head directory)))
    (when head
      (let ((line (ignore-errors
                   (with-open-file (s head :if-does-not-exist nil)
                     (and s (read-line s nil nil))))))
        (cond ((or (null line) (zerop (length line))) nil)
              ((and (>= (length line) 16) (string= "ref: refs/heads/" line :end2 16))
               (string-trim " " (subseq line 16)))
              (t (subseq line 0 (min 7 (length line)))))))))   ; detached: short SHA

(defun prompt-time ()
  "The current wall-clock time as HH:MM:SS."
  (multiple-value-bind (s m h) (decode-universal-time (get-universal-time))
    (format nil "~2,'0D:~2,'0D:~2,'0D" h m s)))

(defun prompt-jobs ()
  "The number of live jobs (for a `[N]` prompt segment)."
  (length (all-jobs)))

(defun prompt-exit-status ()
  "The last foreground command's exit status as a string: \"\" on success (0),
else the code — colorize it in your prompt to taste."
  (if (zerop *last-status*) "" (princ-to-string *last-status*)))

(defparameter +ansi-colors+
  '((:black . 30) (:red . 31) (:green . 32) (:yellow . 33) (:blue . 34)
    (:magenta . 35) (:cyan . 36) (:white . 37)
    (:bright-black . 90) (:bright-red . 91) (:bright-green . 92)
    (:bright-yellow . 93) (:bright-blue . 94) (:bright-magenta . 95)
    (:bright-cyan . 96) (:bright-white . 97))
  "Prompt colour keyword -> ANSI SGR foreground code.")

(defun colorize (string color &optional bold)
  "Wrap STRING in the ANSI SGR code for COLOR (a keyword from +ANSI-COLORS+), plus
bold when BOLD, resetting after.  An unknown COLOR returns STRING unchanged.  The
line editor measures visible width, so a colorized prompt keeps its cursor
correct."
  (let ((code (cdr (assoc color +ansi-colors+))))
    (if code
        (format nil "~C[~:[~;1;~]~Dm~A~C[0m" #\Escape bold code string #\Escape)
        string)))

(defun default-prompt ()
  "consh <cwd-base> [(<git-branch>)]> — showcases the git-branch block; users
replace *prompt-function* to build richer prompts from the PROMPT-* helpers."
  (let ((branch (prompt-git-branch)))
    (format nil "consh ~A~@[ (~A)~]> " (prompt-cwd-base) branch)))

(defvar *prompt-function* #'default-prompt
  "A function of no arguments returning the prompt string.")

(defun prompt () (funcall *prompt-function*))

(defvar *continuation-prompt* "...> "
  "Prompt shown for continuation lines while completing a multi-line input (an
open Lisp form or an unterminated quote).")

;;; ---------------------------------------------------------------------------
;;; Presentation policy: how a REPL result is displayed
;;; ---------------------------------------------------------------------------
;;;
;;; A stream of a wrapped object type (file-info, grep-match, filesystem, ...)
;;; renders as an aligned table by default — the presentation layer's `table`,
;;; which the user would otherwise have to invoke by hand.  Anything else keeps
;;; the plain per-line / readable rendering, so `echo hi` still prints `hi` and a
;;; bare Lisp value prints readably.

(defvar *present-color* nil
  "When true, PRESENT emits table headers bold.  The REPL sets it from terminal
interactivity; bound off for pipes and tests so output stays plain.")

(defun %proper-list-p (x)
  "True when X is a proper (non-dotted, non-circular) list."
  (and (listp x) (ignore-errors (list-length x) t)))

(defun %specialized-columns-p (object)
  "True when OBJECT's type has a TABLE-COLUMNS method more specific than the T
fallback — i.e. it is a wrapped type worth tabulating, not a bare string/number."
  (let ((methods (sb-mop:compute-applicable-methods #'table-columns (list object))))
    (and methods
         (not (eq (first (sb-mop:method-specializers (first methods)))
                  (find-class t))))))

(defun %tabular-result-p (result)
  "True when RESULT should render as a table: a single specialized object, or a
non-empty proper list whose elements are all the same specialized class (a mixed
or unwrapped stream stays per-line)."
  (typecase result
    (null nil)
    (cons (and (%proper-list-p result)
               (%specialized-columns-p (first result))
               (let ((class (class-of (first result))))
                 (every (lambda (x) (eq (class-of x) class)) (rest result)))))
    (t (%specialized-columns-p result))))

(defun present (result &optional (out *standard-output*))
  "Display a REPL RESULT to OUT.  A uniform stream of a wrapped object type
renders as an aligned table (bold header when *PRESENT-COLOR*); any other list
prints one element per line; a scalar prints readably.  Returns no values."
  (cond
    ((%tabular-result-p result)
     (table result :stream out :color *present-color*))
    ((and (listp result) (%proper-list-p result))
     (dolist (x result) (format out "~&~A~%" x)))
    (t (format out "~&~S~%" result)))
  (values))


;;; The REPL (shell-repl / main) lives in lineedit.lisp, which loads after this
;;; file and adds the interactive line editor it uses.
