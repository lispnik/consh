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

(defun %read-word (stream)
  "Read a bare word up to whitespace or a | / & separator.  Embedded quotes are
spliced in literally."
  (with-output-to-string (out)
    (loop for c = (peek-char nil stream nil nil)
          until (or (null c) (member c '(#\Space #\Tab #\Newline #\| #\&)))
          do (if (member c '(#\" #\'))
                 (write-string (%read-quoted stream c) out)
                 (write-char (read-char stream) out)))))

(defun tokenize (line)
  "Tokenize LINE into a list of tokens, each one of:
  (:word . string)   a literal word (quotes stripped),
  (:escape . form)   a Lisp form from `,form` or `$(form)`,
  (:pipe)            a | separator,
  (:amp)             a trailing & (background)."
  (with-input-from-string (s line)
    (let ((tokens '()))
      (loop for c = (peek-char nil s nil nil)
            while c
            do (cond
                 ((member c '(#\Space #\Tab #\Newline)) (read-char s))
                 ((char= c #\|) (read-char s) (push '(:pipe) tokens))
                 ((char= c #\&) (read-char s) (push '(:amp) tokens))
                 ((char= c #\,) (read-char s) (push (cons :escape (read s nil nil)) tokens))
                 ((char= c #\$)
                  (read-char s)
                  (if (eql (peek-char nil s nil nil) #\()
                      (push (cons :escape (read s nil nil)) tokens)   ; $(form)
                      (push (cons :word "$") tokens)))
                 ((member c '(#\" #\')) (push (cons :word (%read-quoted s c)) tokens))
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
;;; Parsing a command line into a Lisp form
;;; ---------------------------------------------------------------------------

(defun %token->form (token)
  (ecase (car token)
    (:word   (cdr token))        ; a self-evaluating string literal
    (:escape (cdr token))))      ; a Lisp form, evaluated at runtime

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
    (cons 'external (mapcar #'%token->form tokens))))

(defun parse-shell-line (line)
  "Desugar a command LINE into a Lisp form that runs it.  NIL for a blank line."
  (let ((tokens (tokenize line)))
    (when (null tokens) (return-from parse-shell-line nil))
    (let* ((background (eq (caar (last tokens)) :amp))
           (tokens (if background (butlast tokens) tokens))
           (stages (%split-pipe tokens)))
      (list '%shell-run
            (cons 'list (mapcar #'%stage->form stages))
            :background background))))

(defun %shell-run (stages &key background)
  "Run the desugared STAGES: collect output (foreground) or start a job (&)."
  (let ((pipeline (make-pipeline stages)))
    (if background
        (run-job pipeline :background t)
        (pipeline-collect pipeline))))

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

;;; ---------------------------------------------------------------------------
;;; A minimal REPL tying it together (interactive; not exercised by tests)
;;; ---------------------------------------------------------------------------

(defun %present (result out)
  (if (listp result)
      (dolist (x result) (format out "~&~A~%" x))
      (format out "~&~S~%" result)))

(defun shell-repl (&key (in *standard-input*) (out *standard-output*))
  "Read-eval-print loop over surface syntax.  Reports pending job events before
each prompt and presents results via print-object.  Ctrl-C aborts the current
line; EOF (Ctrl-D) ends the loop."
  (loop
    (dolist (event (take-job-events)) (format out "~&[~A]~%" event))
    (write-string (prompt) out)
    (finish-output out)
    (let ((line (read-line in nil nil)))
      (when (null line) (return))
      (unless (zerop (length (string-trim '(#\Space #\Tab) line)))
        (handler-case (%present (shell-eval line) out)
          (sb-sys:interactive-interrupt () (format out "~&^C~%"))
          (error (e) (format out "~&Error: ~A~%" e)))))))

(defun main ()
  "Entry point for a dumped consh executable: greet, run the REPL, exit cleanly."
  ;; A saved image baked in *current-directory* at build time; adopt the real
  ;; working directory the executable was launched from.
  (ignore-errors
   (setf *current-directory*
         (truename (pathname (format nil "~A/" (sb-posix:getcwd))))))
  (format t "consh — a Common Lisp Unix shell (objects, not bytes). Ctrl-D to exit.~%")
  (finish-output)
  (handler-case (shell-repl)
    (sb-sys:interactive-interrupt () (terpri)))
  (finish-output)
  (sb-ext:exit :code 0))
