;;;; t/surface.lisp — Phase 6: surface syntax (reader, prompt, history, completion).

(in-package #:consh/test)

(def-suite surface :in consh :description "Surface syntax: reader sugar, prompt, history, completion.")
(in-suite surface)

(defvar *ct-word* "world" "A special var used to exercise a ,escape.")

;;; ===========================================================================
;;; Tokenizer
;;; ===========================================================================

(test tokenize-words
  (is (equal '((:word . "ls") (:word . "-l") (:word . "/tmp"))
             (tokenize "ls -l /tmp"))))

(test tokenize-pipe-and-amp
  (is (equal '((:word . "find") (:word . "/") (:pipe) (:word . "grep") (:word . "foo"))
             (tokenize "find / | grep foo")))
  (is (equal '((:word . "yes") (:amp)) (tokenize "yes &"))))

(test tokenize-quotes-group-and-preserve-spaces
  (is (equal '((:word . "echo") (:word . "hello world"))
             (tokenize "echo 'hello world'")))
  (is (equal '((:word . "echo") (:word . "a b"))
             (tokenize "echo \"a b\""))))

(test tokenize-escapes
  (is (equal '((:word . "echo") (:escape . x)) (tokenize "echo ,x")))
  (is (equal '((:word . "echo") (:escape + 1 2)) (tokenize "echo $(+ 1 2)"))))

(test tokenize-blank-is-empty
  (is (null (tokenize "")))
  (is (null (tokenize "   "))))

;;; ===========================================================================
;;; parse-shell-line desugaring
;;; ===========================================================================

(test parse-single-command
  (is (equal '(%shell-run (list (external "ls" "-l" "/tmp")) :background nil)
             (parse-shell-line "ls -l /tmp"))))

(test parse-pipeline
  (is (equal '(%shell-run (list (external "find" "/") (external "grep" "foo"))
              :background nil)
             (parse-shell-line "find / | grep foo"))))

(test parse-background
  (is (equal '(%shell-run (list (external "yes")) :background t)
             (parse-shell-line "yes &"))))

(test parse-embeds-escape-forms
  (is (equal '(%shell-run (list (external "echo" (+ 1 2))) :background nil)
             (parse-shell-line "echo $(+ 1 2)"))))

(test parse-blank-is-nil
  (is (null (parse-shell-line "")))
  (is (null (parse-shell-line "   "))))

;;; ===========================================================================
;;; Aliases
;;; ===========================================================================

(test alias-expands-in-parse
  (unwind-protect
       (progn
         (define-alias "ll" "ls -l")
         (is (equal '(%shell-run (list (external "ls" "-l" "/tmp")) :background nil)
                    (parse-shell-line "ll /tmp"))))
    (remove-alias "ll")))

(test removed-alias-does-not-expand
  (define-alias "zz" "ls")
  (remove-alias "zz")
  (is (equal '(%shell-run (list (external "zz")) :background nil)
             (parse-shell-line "zz"))))

;;; ===========================================================================
;;; shell-eval — one language for command and Lisp
;;; ===========================================================================

(test eval-runs-a-command
  (is (equal '("hello") (shell-eval "echo hello"))))

(test eval-runs-a-pipeline
  (is (equal '("1" "2" "3") (shell-eval "seq 1 3 | cat"))))

(test foreground-run-without-tty-just-collects
  "Without a controlling terminal, %run-foreground is plain pipeline-collect —
the terminal-handoff path is skipped."
  (let ((*terminal-fd* nil))
    (is (equal '("hi")
               (consh::%run-foreground
                (make-pipeline (list (external "sh" "-c" "printf 'hi\\n'"))))))))

(test eval-dollar-escape-evaluates-lisp
  (is (equal '("HI") (shell-eval "echo $(string-upcase \"hi\")"))))

(test eval-comma-escape-uses-a-lisp-value
  (let ((*package* (find-package :consh/test)))
    (is (equal '("world") (shell-eval "echo ,*ct-word*")))))

(test eval-bare-lisp-form
  (is (= 42 (shell-eval "(+ 40 2)"))))

;;; ---------------------------------------------------------------------------
;;; User init file
;;; ---------------------------------------------------------------------------

(test init-file-path-honors-xdg-config-home
  (let ((saved (sb-ext:posix-getenv "XDG_CONFIG_HOME")))
    (unwind-protect
         (progn
           (sb-posix:setenv "XDG_CONFIG_HOME" "/tmp/consh-xdg" 1)
           (is (equal "/tmp/consh-xdg/consh/consh.lisp"
                      (namestring (init-file-path)))))
      (if saved (sb-posix:setenv "XDG_CONFIG_HOME" saved 1)
          (sb-posix:unsetenv "XDG_CONFIG_HOME")))))

(test init-file-path-defaults-under-home
  (let ((saved (sb-ext:posix-getenv "XDG_CONFIG_HOME")))
    (unwind-protect
         (progn
           (sb-posix:unsetenv "XDG_CONFIG_HOME")
           (is (search ".config/consh/consh.lisp" (namestring (init-file-path)))))
      (when saved (sb-posix:setenv "XDG_CONFIG_HOME" saved 1)))))

(test load-init-file-missing-returns-nil
  (is (null (load-init-file :path #p"/consh-nonexistent-dir-xyz/consh.lisp"))))

(test load-init-file-evaluates-in-consh-package
  "The init file is loaded in the CONSH package, so it configures the shell —
here it defines an alias, unqualified."
  (let* ((dir (make-temp-dir))
         (file (merge-pathnames "consh.lisp" dir)))
    (with-open-file (s file :direction :output)
      (write-line "(define-alias \"initgrep\" \"grep -i\")" s))
    (unwind-protect
         (progn
           (is-true (load-init-file :path file))
           (is (equal "grep -i" (gethash "initgrep" *aliases*))))
      (remove-alias "initgrep"))))

(test load-init-file-swallows-errors
  "A broken init file is reported and swallowed — never fatal to startup."
  (let* ((dir (make-temp-dir))
         (file (merge-pathnames "consh.lisp" dir)))
    (with-open-file (s file :direction :output)
      (write-line "(error \"boom in init file\")" s))
    ;; returns NIL and does not signal
    (is (null (let ((*error-output* (make-broadcast-stream)))   ; hush the report
                (load-init-file :path file))))))

(test eval-lisp-line-reaches-consh-vocabulary
  "A Lisp line read in the CONSH package (as the dumped-image REPL binds it) can
name the shell's own vocabulary unqualified — pipe, pipeline-collect, external."
  (let ((*package* (find-package '#:consh)))
    (is (equal '("a" "b")
               (shell-eval
                "(pipeline-collect (pipe (:generate (emit) (funcall emit \"a\") (funcall emit \"b\"))))")))))

(test eval-background-returns-a-job
  (let ((job (shell-eval "echo bgtest &")))
    (is (typep job 'job))
    (is (equal '("bgtest") (fg job)))))

;;; ===========================================================================
;;; History: (form . result), results hold live objects
;;; ===========================================================================

(test history-records-form-and-result
  (clear-history)
  (shell-eval "echo one")
  (shell-eval "(+ 2 3)")
  (is (= 2 (history-count)))
  (is (equal '(%shell-run (list (external "echo" "one")) :background nil)
             (history-form 0)))
  (is (equal '("one") (history-result 0)))
  (is (= 5 (history-result 1)))
  (is (= 5 (last-result))))

(test history-holds-live-objects
  "The recorded result is the very object list produced, not a re-render."
  (clear-history)
  (let ((result (shell-eval "echo alive")))
    (is (eq result (history-result 0)))))               ; identity preserved

(test clear-history-empties
  (shell-eval "echo x")
  (clear-history)
  (is (= 0 (history-count)))
  (is (null (last-result))))

;;; ===========================================================================
;;; Prompt is a function
;;; ===========================================================================

(test default-prompt-mentions-cwd
  "The default prompt names the current directory (its final component)."
  (let ((*current-directory* #P"/tmp/consh-prompt-xyz/"))
    (is (search "consh-prompt-xyz" (prompt)))))

(test prompt-function-is-customizable
  (let ((*prompt-function* (lambda () "$$ ")))
    (is (string= "$$ " (prompt)))))

;;; ---------------------------------------------------------------------------
;;; Prompt building blocks + colour
;;; ---------------------------------------------------------------------------

(test prompt-cwd-uses-current-directory
  "PROMPT-CWD-BASE is the final path component; PROMPT-CWD the whole path."
  (let ((*current-directory* #P"/tmp/consh-cwd-test/deep/"))
    (is (string= "deep" (prompt-cwd-base)))
    (is (search "consh-cwd-test/deep" (prompt-cwd)))))

(test prompt-cwd-abbreviates-home
  "A directory under $HOME is abbreviated with a leading tilde."
  (let* ((home (user-homedir-pathname))
         (*current-directory* (merge-pathnames "consh-home-probe/" home)))
    (is (char= #\~ (char (prompt-cwd) 0)))))

(test prompt-user-and-host-are-nonempty
  (is (plusp (length (prompt-user))))
  (is (plusp (length (prompt-host))))
  ;; host name carries no dotted domain suffix
  (is (not (find #\. (prompt-host)))))

(test prompt-git-branch-reads-head
  "PROMPT-GIT-BRANCH resolves the symbolic ref in .git/HEAD, walking upward."
  (let* ((root (merge-pathnames (format nil "consh-git-~D/" (sb-posix:getpid))
                                #P"/tmp/"))
         (sub  (merge-pathnames "a/b/" root))
         (head (merge-pathnames ".git/HEAD" root)))
    (unwind-protect
         (progn
           (ensure-directories-exist head)
           (ensure-directories-exist sub)
           (with-open-file (s head :direction :output :if-exists :supersede)
             (write-string "ref: refs/heads/testbranch" s))
           ;; found from the repo root and from a nested subdirectory
           (is (string= "testbranch" (prompt-git-branch root)))
           (is (string= "testbranch" (prompt-git-branch sub)))
           ;; a detached HEAD (bare SHA) shortens to 7 chars
           (with-open-file (s head :direction :output :if-exists :supersede)
             (write-string "0123456789abcdef0123456789abcdef01234567" s))
           (is (string= "0123456" (prompt-git-branch root))))
      (ignore-errors (uiop:delete-directory-tree (truename root)
                                                 :validate t)))))

(test prompt-git-branch-nil-outside-repo
  (let ((dir (merge-pathnames (format nil "consh-nogit-~D/" (sb-posix:getpid))
                              #P"/tmp/")))
    (unwind-protect
         (progn
           (ensure-directories-exist dir)
           (is (null (prompt-git-branch dir))))
      (ignore-errors (uiop:delete-directory-tree (truename dir) :validate t)))))

(test prompt-time-is-hh-mm-ss
  (let ((s (prompt-time)))
    (is (= 8 (length s)))
    (is (char= #\: (char s 2)))
    (is (char= #\: (char s 5)))
    (is (every (lambda (c) (or (digit-char-p c) (char= c #\:))) s))))

(test prompt-jobs-counts-jobs
  (is (= (length (all-jobs)) (prompt-jobs))))

(test prompt-exit-status-reflects-last-status
  (let ((*last-status* 0))
    (is (string= "" (prompt-exit-status))))
  (let ((*last-status* 42))
    (is (string= "42" (prompt-exit-status)))))

(test colorize-wraps-in-sgr
  "COLORIZE brackets the text with an SGR set and a reset; bold adds the 1; prefix."
  (let ((red (colorize "abc" :red)))
    (is (string= (format nil "~C[31mabc~C[0m" #\Escape #\Escape) red))
    ;; the visible width is unchanged by the escapes
    (is (= 3 (consh::%display-width red))))
  (let ((bold (colorize "X" :green t)))
    (is (string= (format nil "~C[1;32mX~C[0m" #\Escape #\Escape) bold)))
  ;; an unknown colour passes the text through untouched
  (is (string= "plain" (colorize "plain" :chartreuse))))

;;; ===========================================================================
;;; Completion is a generic function
;;; ===========================================================================

(test complete-command-includes-wrappers
  (let ((cs (complete :command "")))
    (dolist (w '("ls" "find" "cat" "grep"))
      (is (member w cs :test #'string=))))
  ;; and filters by prefix
  (is (member "ls" (complete :command "l") :test #'string=))
  (is (not (member "cat" (complete :command "l") :test #'string=))))

(test complete-symbol-in-package
  (let ((cs (complete :symbol "lis" :package :common-lisp)))
    (is (member "list" cs :test #'string=))
    (is (member "list-length" cs :test #'string=))
    (is (every (lambda (s) (eql 0 (search "lis" s))) cs))))

(test complete-path-matches-prefix
  (let ((dir (make-temp-dir)))
    (dolist (n '("apple.txt" "apricot.txt" "banana.txt"))
      (with-open-file (s (merge-pathnames n dir) :direction :output) (write-string "x" s)))
    (let ((cs (complete :path "ap" :directory dir)))
      (is (member "apple.txt" cs :test #'string=))
      (is (member "apricot.txt" cs :test #'string=))
      (is (not (member "banana.txt" cs :test #'string=))))))

(test complete-line-picks-context
  ;; first token -> command
  (is (member "ls" (complete-line "l") :test #'string=))
  ;; a ,-led token -> symbol
  (let ((*package* (find-package :common-lisp-user)))
    (is (member "list" (complete-line "echo ,lis") :test #'string=)))
  ;; a later plain token -> path (in a known directory)
  (let ((dir (make-temp-dir)))
    (with-open-file (s (merge-pathnames "zzfile" dir) :direction :output) (write-string "x" s))
    (let ((*current-directory* dir))
      (is (member "zzfile" (complete-line "cat zz") :test #'string=)))))

;;; --- multi-line continuation ----------------------------------------------

(test input-complete-p-detects-incomplete-prefixes
  ;; complete inputs
  (is-true  (input-complete-p "ls -l"))
  (is-true  (input-complete-p "(+ 1 2)"))
  (is-true  (input-complete-p "echo \"closed\""))
  (is-true  (input-complete-p ""))
  ;; incomplete: an open Lisp form or an unterminated quote wants more input
  (is-false (input-complete-p "(+ 1 2"))
  (is-false (input-complete-p "(list (foo"))
  (is-false (input-complete-p "echo \"open"))
  ;; a genuine parse error counts as complete (so it gets reported, not extended)
  (is-true  (input-complete-p "echo )bad( )")))

(test incomplete-input-is-a-parse-error-subtype
  ;; batch/programmatic use still sees a shell-parse-error
  (signals shell-parse-error (shell-eval "(+ 1 2"))
  (signals shell-input-incomplete (shell-eval "echo \"open")))

(test multi-line-lisp-form-evaluates-when-closed
  ;; the accumulated buffer (joined with newlines) parses and runs as one form
  (is (equal '(1 2 3) (shell-eval (format nil "(list 1~%2~%3)")))))

(test complete-command-includes-aliases
  (let ((*aliases* (make-hash-table :test 'equal)))
    (define-alias "zzalias" "ls -l")
    (is (member "zzalias" (complete :command "zz") :test #'string=))
    (is (member "zzalias" (complete-line "zz") :test #'string=))))

(test complete-env-var-names
  (sb-posix:setenv "CONSH_TEST_ZZVAR" "1" 1)
  (unwind-protect
       (progn
         (is (member "CONSH_TEST_ZZVAR" (complete :env "CONSH_TEST_ZZ") :test #'string=))
         ;; $-led tokens complete to $-prefixed names via complete-line
         (is (member "$CONSH_TEST_ZZVAR" (complete-line "echo $CONSH_TEST_ZZ")
                     :test #'string=)))
    (ignore-errors (sb-posix:unsetenv "CONSH_TEST_ZZVAR"))))

(test complete-path-expands-leading-tilde
  ;; a directory under $HOME completes with the ~ kept in the result
  (let ((probe (merge-pathnames "consh-tildecomp-XYZ/" (user-homedir-pathname))))
    (unwind-protect
         (progn (ensure-directories-exist probe)
                (is (member "~/consh-tildecomp-XYZ/"
                            (complete :path "~/consh-tildecomp-XY") :test #'string=)))
      (ignore-errors (uiop:delete-directory-tree (truename probe) :validate t)))))

;;; ===========================================================================
;;; $VAR / ${VAR} environment expansion
;;; ===========================================================================

(test expand-vars-basic
  (sb-posix:setenv "CONSH_TV" "world" 1)
  (unwind-protect
       (progn
         (is (string= "hi-world" (%expand-vars "hi-$CONSH_TV")))
         (is (string= "[world]" (%expand-vars "[${CONSH_TV}]")))
         (is (string= "world/x" (%expand-vars "$CONSH_TV/x")))
         (is (string= "" (%expand-vars "$CONSH_UNSET_ZZ")))       ; unset -> empty
         (is (string= "$ 5" (%expand-vars "$ 5"))))               ; bare $ literal
    (sb-posix:unsetenv "CONSH_TV")))

(test expand-vars-in-command
  (sb-posix:setenv "CONSH_TV" "there" 1)
  (unwind-protect
       (is (equal '("hi there") (shell-eval "echo hi $CONSH_TV")))
    (sb-posix:unsetenv "CONSH_TV")))

;;; ===========================================================================
;;; Script positional parameters + option parsing + script evaluation
;;; ===========================================================================

(test positional-parameter-expansion
  (let ((*script-name* "myscript") (*script-args* '("a" "b" "c")) (*last-status* 0))
    (is (string= "myscript" (%expand-vars "$0")))
    (is (string= "a"  (%expand-vars "$1")))
    (is (string= "c"  (%expand-vars "$3")))
    (is (string= ""   (%expand-vars "$4")))               ; out of range -> empty
    (is (string= "a"  (%expand-vars "${1}")))              ; braced form
    (is (string= "3"  (%expand-vars "$#")))                ; arg count
    (is (string= "a b c" (%expand-vars "$@")))
    (is (string= "a b c" (%expand-vars "$*")))
    (is (string= "0"  (%expand-vars "$?")))                ; last status
    (is (string= "3"  (%expand-vars "${#}")))
    (is (string= "arg=a!" (%expand-vars "arg=$1!")))       ; mixed with literals
    ;; a bare $<digits> is the whole number
    (is (string= "" (let ((*script-args* '("x"))) (%expand-vars "$10"))))))

(test positional-parameters-default-outside-a-script
  (let ((*script-name* nil) (*script-args* nil) (*last-status* 7))
    (is (string= "consh" (%expand-vars "$0")))
    (is (string= "0" (%expand-vars "$#")))
    (is (string= "" (%expand-vars "$@")))
    (is (string= "" (%expand-vars "$1")))
    (is (string= "7" (%expand-vars "$?")))))

(test parse-args-options-and-positionals
  (multiple-value-bind (opts pos)
      (parse-args '("-v" "--out" "o.mp4" "in1" "in2")
                  '((:verbose "-v" "--verbose" boolean) (:out "-o" "--out" string)))
    (is (eq t (getf opts :verbose)))
    (is (string= "o.mp4" (getf opts :out)))
    (is (equal '("in1" "in2") pos)))
  ;; --opt=val and -o=val forms
  (multiple-value-bind (opts pos)
      (parse-args '("--out=x" "-n=5" "p")
                  '((:out "--out" string) (:num "-n" string)))
    (is (string= "x" (getf opts :out)))
    (is (string= "5" (getf opts :num)))
    (is (equal '("p") pos)))
  ;; `--` ends option parsing
  (multiple-value-bind (opts pos)
      (parse-args '("-v" "--" "-notanoption") '((:verbose "-v" boolean)))
    (is (eq t (getf opts :verbose)))
    (is (equal '("-notanoption") pos)))
  ;; errors: unknown option, missing value
  (signals shell-parse-error (parse-args '("-x") '((:v "-v" boolean))))
  (signals shell-parse-error (parse-args '("-o") '((:o "-o" string)))))

(test eval-script-lines-skips-shebang-and-accumulates-multiline
  (let* ((nl (string #\Newline))
         (script (concatenate 'string
                              "#!/x/consh" nl "# a comment" nl nl
                              "(format t \"sum=~A\" (+ 1" nl "   2" nl "   3))" nl))
         (in (make-string-input-stream script))
         (out (with-output-to-string (*standard-output*)
                (%eval-script-lines (lambda () (read-line in nil nil))))))
    (is (string= "sum=6" out))))                          ; shebang/comment/blank skipped, form joined

(test eval-script-lines-propagates-exit
  (let ((in (make-string-input-stream (format nil "exit 5~%echo after~%"))))
    (handler-case (progn (%eval-script-lines (lambda () (read-line in nil nil)))
                         (fail "exit did not propagate"))
      (shell-exit (c) (is (= 5 (shell-exit-code c)))))))

;;; ===========================================================================
;;; Globbing
;;; ===========================================================================

(test glob-matches-star-question-set
  (let ((dir (make-temp-dir)))
    (dolist (n '("a.txt" "b.txt" "c.log" "a1" "a2" "ab"))
      (with-open-file (s (merge-pathnames n dir) :direction :output) (write-string "x" s)))
    (is (equal '("a.txt" "b.txt")
               (mapcar #'file-namestring (glob "*.txt" :directory dir))))
    (is (equal '("a1" "a2" "ab")                                ; ? = exactly one char
               (mapcar #'file-namestring (glob "a?" :directory dir))))
    (is (equal '("a1" "a2")                                     ; [set]
               (mapcar #'file-namestring (glob "a[12]" :directory dir))))))

(test glob-in-a-command-expands-to-args
  (let ((dir (make-temp-dir)))
    (dolist (n '("one.txt" "two.txt" "skip.md"))
      (with-open-file (s (merge-pathnames n dir) :direction :output) (write-string "x" s)))
    (let ((*current-directory* dir))
      ;; cat *.txt -> the two files concatenated (each holds "x", no newline)
      (is (equal '("xx") (shell-eval "cat *.txt"))))))

(test glob-no-match-stays-literal
  (let ((dir (make-temp-dir)))
    (let ((*current-directory* dir))
      (is (equal '("nope-*.zzz") (shell-eval "echo nope-*.zzz"))))))

(test glob-subdirectory-pattern
  (let ((dir (make-temp-dir)))
    (ensure-directories-exist (merge-pathnames "sub/" dir))
    (with-open-file (s (merge-pathnames "sub/x.txt" dir) :direction :output) (write-string "x" s))
    (is (equal '("x.txt")
               (mapcar #'file-namestring (glob "sub/*.txt" :directory dir))))))

(test glob-expands-directory-components
  "A glob in a NON-final path component (`*/x`, `d*/x`) is expanded, and a
trailing `/` (`*/`) restricts matches to directories."
  (let ((dir (make-temp-dir)))
    (ensure-directories-exist (merge-pathnames "d1/" dir))
    (ensure-directories-exist (merge-pathnames "d2/" dir))
    (dolist (p '("d1/x.txt" "d2/x.txt" "top.txt"))
      (with-open-file (s (merge-pathnames p dir) :direction :output) (write-string "x" s)))
    ;; `*/x.txt` finds x.txt in every subdirectory
    (is (equal '("x.txt" "x.txt")
               (mapcar #'file-namestring (glob "*/x.txt" :directory dir))))
    (is (equal '("x.txt" "x.txt")
               (mapcar #'file-namestring (glob "d*/x.txt" :directory dir))))
    ;; `*/` matches only directories (not the plain file top.txt)
    (is (equal '("d1" "d2")
               (mapcar (lambda (p) (car (last (pathname-directory p))))
                       (glob "*/" :directory dir))))))

(test glob-returns-relative-paths
  "A relative pattern yields RELATIVE pathnames (like a shell) so they resolve
against a spawned child's chdir'd cwd; an absolute pattern stays absolute."
  (let ((dir (make-temp-dir)))
    (ensure-directories-exist (merge-pathnames "sub/" dir))
    (dolist (p '("a.txt" "sub/x.txt"))
      (with-open-file (s (merge-pathnames p dir) :direction :output) (write-string "x" s)))
    (is (equal '("a.txt") (mapcar #'namestring (glob "*.txt" :directory dir))))
    (is (equal '("sub/x.txt") (mapcar #'namestring (glob "sub/*.txt" :directory dir))))
    ;; an absolute pattern keeps absolute results
    (is (every (lambda (p) (eq :absolute (car (pathname-directory p))))
               (glob (concatenate 'string (namestring dir) "*.txt"))))))

;;; ---------------------------------------------------------------------------
;;; Parser hardening: malformed input yields a clean error (or literal), never a
;;; raw internal condition or a crash.
;;; ---------------------------------------------------------------------------

(test glob-unclosed-bracket-is-literal-not-a-crash
  "An unterminated [ set matches the literal `[` instead of indexing off the end
of the pattern."
  (is-true (consh::%glob-match-p "[abc" "[abc"))
  (is-false (consh::%glob-match-p "[abc" "abc"))
  (is-false (consh::%glob-match-p "a[b" "axb")))   ; no crash on the interior [

(test glob-malformed-pattern-matches-nothing
  "A pattern that is not a valid pathname yields no matches (kept literal),
never a raw NAMESTRING-PARSE-ERROR / TYPE-ERROR."
  (finishes (glob "[^/bbc"))
  (finishes (glob "/*/, -&"))
  (is (null (glob "[^/no-such-zzz"))))

(test malformed-escape-form-is-a-clean-parse-error
  (signals shell-parse-error (tokenize "echo $(+ 1 2"))    ; unbalanced parens
  (signals shell-parse-error (tokenize "echo ,#$foo")))    ; reader error

(test stray-ampersand-is-a-clean-parse-error
  "A `&` that is not the trailing background marker is a clean error, not a raw
CASE-FAILURE."
  (signals shell-parse-error (parse-shell-line "echo & foo")))

(test unterminated-quote-is-a-clean-parse-error
  "An unterminated quote is a clean shell-parse-error, not silently accepted."
  (signals shell-parse-error (tokenize "echo \"hi"))
  (signals shell-parse-error (parse-shell-line "echo 'x"))
  ;; a properly terminated quote still groups into one word
  (is (equal '((:word . "echo") (:word . "a b")) (tokenize "echo \"a b\""))))

(test adversarial-inputs-never-raise-raw-errors
  "A batch of nasty lines must each parse or raise shell-parse-error — never a
raw internal condition."
  (dolist (line '("[" "]" "[a-" "[!]" "${" "${}" "$" "$(" ",(" "\"" "'" "<" ">"
                  ">>" "2>" "| |" "& &" "a$b" "***" "*?[*" "/[};'" "echo `x"
                  "[z-a]" "sub/[" "$()" ",)" "a\"b'c" "  |  "))
    (handler-case (progn (tokenize line) (parse-shell-line line))
      (shell-parse-error () t)
      (serious-condition (e)
        (fail "line ~S leaked a raw ~A" line (type-of e))))))

(test glob-many-stars-is-linear-not-exponential
  "A pattern of many `*` against a long non-matching name must return promptly
(linear matcher).  If the exponential recursion regressed, this test would hang
the whole suite."
  (is-false (consh::%glob-match-p (concatenate 'string (make-string 25 :initial-element #\*) "b")
                                  (make-string 60 :initial-element #\a)))
  (is-true (consh::%glob-match-p "foo*bar*baz*qux" "fooAbarBbazCqux"))
  (is-false (consh::%glob-match-p "foo*bar*baz*qux" "fooAbarBbazCquux")))

(test basename-of-root-is-a-string-not-a-keyword
  "%basename must never return the :ABSOLUTE marker (which would crash the glob
matcher's (length name))."
  (is (stringp (consh::%basename #p"/"))))

(test lisp-line-unbalanced-or-trailing-is-a-clean-error
  (signals shell-parse-error (shell-eval "(+ 1 2" :record nil))       ; unbalanced
  (signals shell-parse-error (shell-eval "(+ 1 2) rm -rf /" :record nil)) ; trailing
  ;; a well-formed single form still evaluates
  (is (= 3 (shell-eval "(+ 1 2)" :record nil))))

;;; ===========================================================================
;;; Builtins
;;; ===========================================================================

(test builtin-p-recognizes-builtins
  (is-true (builtin-p "cd"))
  (is-true (builtin-p "pwd"))
  (is-false (builtin-p "ls")))                                 ; a wrapper, not a builtin

(test cd-changes-current-directory-not-process-cwd
  (let* ((start (make-temp-dir))
         (target (make-temp-dir))
         (*current-directory* start)
         (*previous-directory* nil)
         (cwd-before (sb-posix:getcwd)))
    (shell-eval (format nil "cd ~A" (namestring target)))
    (is (equal (truename target) *current-directory*))
    (is (equal cwd-before (sb-posix:getcwd)))                  ; process cwd untouched
    ;; cd - returns to the previous directory
    (shell-eval "cd -")
    (is (equal (truename start) *current-directory*))))

(test cd-to-missing-directory-errors
  (let ((*current-directory* (make-temp-dir)))
    (signals shell-parse-error (shell-eval "cd consh-no-such-subdir-xyz"))))

(test pwd-builtin
  (let ((dir (make-temp-dir)))
    (let ((*current-directory* dir))
      (is (string= (namestring dir) (shell-eval "pwd"))))))

;;; --- tilde expansion ------------------------------------------------------

(test tilde-expands-to-home
  (let ((home (string-right-trim "/" (namestring (user-homedir-pathname)))))
    (is (string= home (consh::%expand-tilde "~")))
    (is (string= (concatenate 'string home "/notes") (consh::%expand-tilde "~/notes")))
    (is (string= "plain" (consh::%expand-tilde "plain")))          ; no leading ~
    (is (string= "~nosuchuser-zzz/x" (consh::%expand-tilde "~nosuchuser-zzz/x"))))) ; unknown user

(test tilde-expands-in-command-args
  ;; the whole arg pipeline expands a leading ~ (so `cat ~/x` works)
  (let ((home (string-right-trim "/" (namestring (user-homedir-pathname)))))
    (is (equal (list (concatenate 'string home "/a"))
               (consh::%expand-word-arg "~/a")))))

;;; --- directory stack + auto-cd --------------------------------------------

(test pushd-popd-maintain-a-directory-stack
  (let* ((base (make-temp-dir))
         (sub (merge-pathnames "sub/" base))
         (*current-directory* base)
         (*previous-directory* nil)
         (*dir-stack* '()))
    (ensure-directories-exist sub)
    (shell-eval "pushd sub")
    (is (search "sub" (namestring *current-directory*)))
    (is (= 1 (length *dir-stack*)))
    (shell-eval "popd")
    (is (equal (truename base) *current-directory*))
    (is (null *dir-stack*))
    (signals shell-parse-error (shell-eval "popd"))))              ; empty stack

(test auto-cd-changes-into-a-bare-directory-name
  (let* ((base (make-temp-dir))
         (sub (merge-pathnames "workspace/" base))
         (*current-directory* base)
         (*previous-directory* nil))
    (ensure-directories-exist sub)
    ;; a lone existing-directory name desugars to a `cd` builtin
    (let ((form (parse-shell-line "workspace")))
      (is (eq '%builtin (first form)))
      (is (string= "cd" (second form))))
    (shell-eval "workspace")
    (is (equal (truename sub) *current-directory*))
    ;; a non-directory word is NOT auto-cd'd
    (is (not (eq '%builtin (first (parse-shell-line "definitely-not-a-dir-zzz")))))
    ;; and auto-cd can be turned off
    (let ((*auto-cd* nil))
      (is (not (eq '%builtin (first (parse-shell-line "workspace"))))))))

;;; --- help / type / which / source ----------------------------------------

(test help-lists-builtins-and-describes-one
  (is (search "change directory" (first (%builtin "help" '("cd")))))
  (is (search "not a builtin" (first (%builtin "help" '("bogus-zzz")))))
  (let ((all (%builtin "help" nil)))
    (is (search "consh builtins" (first all)))
    (is (some (lambda (l) (search "pushd" l)) all))))

(test type-classifies-names
  (is (search "shell builtin" (first (%builtin "type" '("cd")))))
  (is (search "not found" (first (%builtin "type" '("no-such-cmd-zzz")))))
  (let ((*aliases* (make-hash-table :test 'equal)))
    (define-alias "zz" "ls -l")
    (is (search "aliased" (first (%builtin "type" '("zz")))))))

(test which-and-type-never-leak-on-wild-names
  "A name that parses as a wild pathname (e.g. \"[!]\") must not crash $PATH
lookup — probe-file/truename would signal on it."
  (finishes (consh::%find-on-path "[!]"))
  (finishes (%builtin "which" '("[!]" "a[b]c")))
  (finishes (%builtin "type" '("[!]"))))

(test source-runs-each-line-of-a-script
  (let ((path (merge-pathnames (format nil "consh-src-~D.consh" (sb-posix:getpid)) #P"/tmp/"))
        (*aliases* (make-hash-table :test 'equal))
        (*current-directory* (truename #P"/tmp/")))
    (unwind-protect
         (progn
           (with-open-file (s path :direction :output :if-exists :supersede)
             (write-line "# a comment line is skipped" s)
             (write-line "(define-alias \"gg\" \"grep\")" s)
             (write-line "" s))
           (shell-eval (format nil "source ~A" (namestring path)))
           (is (string= "grep" (gethash "gg" *aliases*))))
      (ignore-errors (delete-file path)))
    (signals shell-parse-error (shell-eval "source /no/such/file/zzz.consh"))))

(test export-and-unset
  (unwind-protect
       (progn
         (shell-eval "export CONSH_EXP=99")
         (is (string= "99" (sb-ext:posix-getenv "CONSH_EXP")))
         (shell-eval "unset CONSH_EXP")
         (is (null (sb-ext:posix-getenv "CONSH_EXP"))))
    (sb-posix:unsetenv "CONSH_EXP")))

(test alias-builtin-defines-and-lists
  (unwind-protect
       (progn
         (shell-eval "alias cg=grep")
         (is (equal '(%shell-run (list (external "grep" "x")) :background nil)
                    (parse-shell-line "cg x")))
         (is (member "cg=grep" (shell-eval "alias") :test #'string=)))
    (remove-alias "cg")))

(test exit-builtin-signals-shell-exit
  (is (= 5 (handler-case (progn (shell-eval "exit 5") -1)
             (shell-exit (c) (shell-exit-code c))))))

(test jobs-and-fg-builtins
  (let ((job (run-job (make-pipeline (list (external "sh" "-c" "printf 'z\\n'"))) :background t)))
    (is (member job (shell-eval "jobs")))
    (is (equal '("z") (shell-eval (format nil "fg %~D" (job-id job)))))))

(test wait-builtin-returns-job-output
  (let ((job (run-job (make-pipeline (list (external "sh" "-c" "printf 'q\\n'"))) :background t)))
    (is (equal '("q") (shell-eval (format nil "wait %~D" (job-id job)))))))

(test kill-builtin-terminates-a-job
  (let ((job (run-job (make-pipeline (list (external "sleep" "30"))) :background t)))
    (is (member job (shell-eval "jobs")))
    (shell-eval (format nil "kill %~D" (job-id job)))
    (is (eq :done (job-state job)))
    (is-true (job-complete-p job))))

(test kill-parses-signal-names-and-numbers
  (is (= consh::+sigkill+ (consh::%parse-signal "9")))
  (is (= consh::+sigkill+ (consh::%parse-signal "KILL")))
  (is (= consh::+sigkill+ (consh::%parse-signal "SIGKILL")))
  (is (= consh::+sigterm+ (consh::%parse-signal "term")))
  ;; a leading -SIG sets the signal; default is SIGTERM
  (multiple-value-bind (sig targets) (consh::%parse-kill-args '("-9" "%1"))
    (is (= consh::+sigkill+ sig))
    (is (equal '("%1") targets)))
  (multiple-value-bind (sig targets) (consh::%parse-kill-args '("%2"))
    (is (= consh::+sigterm+ sig))
    (is (equal '("%2") targets))))

(test kill-unknown-signal-and-missing-target-error
  (signals shell-parse-error (consh::%parse-signal "BOGUS"))
  (signals shell-parse-error (%builtin "kill" '())))

(test kill-illegal-pid-reports-cleanly-and-still-kills-valid-targets
  "A malformed pid raises a clean shell-parse-error (not a raw parse-integer
error), and any valid targets before it are still signalled."
  (signals shell-parse-error (%builtin "kill" '("notapid")))
  (let ((job (run-job (make-pipeline (list (external "sleep" "30"))) :background t)))
    (signals shell-parse-error
      (%builtin "kill" (list (format nil "%~D" (job-id job)) "notapid")))
    ;; the job was killed before the bad target aborted the builtin
    (is (eq :done (job-state job)))))

(test builtins-only-dispatch-single-stage-foreground
  ;; `cd` inside a pipeline is NOT a builtin — it desugars to an external stage
  (is (equal '(%shell-run (list (external "cd" "x") (external "cat")) :background nil)
             (parse-shell-line "cd x | cat"))))

(test history-builtin-lists-past-forms
  (clear-history)
  (shell-eval "(+ 1 1)")
  (let ((h (shell-eval "history")))
    (is (consp h))
    (is (equal '(+ 1 1) (cdr (first h))))))

(test command-completion-includes-builtins
  (is (member "cd" (complete :command "c") :test #'string=))
  (is (member "pwd" (complete :command "pw") :test #'string=)))

;;; ===========================================================================
;;; Redirections:  >  >>  <  2>  2>>
;;; ===========================================================================

(test tokenize-redirects
  (is (equal '((:word . "echo") (:word . "hi") (:redir . :out) (:word . "f"))
             (tokenize "echo hi > f")))
  (is (equal '((:redir . :out-append) (:word . "f")) (tokenize ">> f")))
  (is (equal '((:redir . :in) (:word . "f")) (tokenize "< f")))
  (is (equal '((:word . "c") (:redir . :err) (:word . "f")) (tokenize "c 2> f")))
  (is (equal '((:word . "c") (:redir . :err-append) (:word . "f")) (tokenize "c 2>> f")))
  ;; a digit followed by a SPACE then > is a plain arg, not an fd redirect
  (is (equal '((:word . "echo") (:word . "2") (:redir . :out) (:word . "f"))
             (tokenize "echo 2 > f"))))

(test parse-attaches-redirections
  (is (equal '(%shell-run (list (external "echo" "hi" :redirections '((:out . "f"))))
              :background nil)
             (parse-shell-line "echo hi > f"))))

(test redirect-stdout-to-file
  (let ((dir (make-temp-dir)))
    (let ((*current-directory* dir))
      (is (null (shell-eval "echo hello-out > out.txt")))     ; output went to file
      (is (string= "hello-out"
                   (with-open-file (s (merge-pathnames "out.txt" dir)) (read-line s)))))))

(test redirect-append
  (let ((dir (make-temp-dir)))
    (let ((*current-directory* dir))
      (shell-eval "echo one > f.txt")
      (shell-eval "echo two >> f.txt")
      (is (equal '("one" "two")
                 (with-open-file (s (merge-pathnames "f.txt" dir))
                   (list (read-line s) (read-line s))))))))

(test redirect-stdin-from-file
  (let ((dir (make-temp-dir)))
    (with-open-file (s (merge-pathnames "in.txt" dir) :direction :output)
      (write-line "keep me" s) (write-line "drop" s))
    (let ((*current-directory* dir))
      ;; grep is rewritten to -n, so it yields a grep-match (line 1, "keep me")
      (is (equal '("keep me")
                 (mapcar #'grep-match-text (shell-eval "grep keep < in.txt")))))))

(test redirect-stderr-to-file
  (let ((dir (make-temp-dir)))
    (let ((*current-directory* dir))
      (shell-eval "sh -c 'echo oops 1>&2' 2> err.txt")
      (is (string= "oops"
                   (with-open-file (s (merge-pathnames "err.txt" dir)) (read-line s)))))))

(test redirect-target-expands-vars
  (let ((dir (make-temp-dir)))
    (sb-posix:setenv "CONSH_OUT" "v.txt" 1)
    (unwind-protect
         (let ((*current-directory* dir))
           (shell-eval "echo x > $CONSH_OUT")
           (is (probe-file (merge-pathnames "v.txt" dir))))
      (sb-posix:unsetenv "CONSH_OUT"))))

(test redirect-honors-current-directory
  "A relative redirect target is created in *current-directory*, not the process
cwd."
  (let ((dir (make-temp-dir))
        (cwd-before (sb-posix:getcwd)))
    (let ((*current-directory* dir))
      (shell-eval "echo hi > rel.txt"))
    (is (probe-file (merge-pathnames "rel.txt" dir)))
    (is (equal cwd-before (sb-posix:getcwd)))))

;;; --- fd duplication: 2>&1, >&2, &>file --------------------------------------

(test tokenize-fd-duplication
  (is (equal '((:word . "cmd") (:redir-dup 2 1)) (tokenize "cmd 2>&1")))
  (is (equal '((:word . "cmd") (:redir-dup 1 2)) (tokenize "cmd >&2")))
  (is (equal '((:word . "cmd") (:redir-dup 1 2)) (tokenize "cmd 1>&2")))
  (is (equal '((:word . "cmd") (:redir-both . :trunc) (:word . "f")) (tokenize "cmd &>f")))
  (is (equal '((:word . "cmd") (:redir-both . :append) (:word . "f")) (tokenize "cmd &>>f")))
  ;; a plain trailing & is still background, not a redirect
  (is (equal '((:word . "cmd") (:amp)) (tokenize "cmd &"))))

(test parse-fd-duplication-specs
  (is (equal '(%shell-run (list (external "cmd" :redirections '((:dup 2 1)))) :background nil)
             (parse-shell-line "cmd 2>&1")))
  ;; &>f  ->  stdout to f, then stderr dup'd from stdout, in order
  (is (equal '(%shell-run (list (external "cmd" :redirections '((:out . "f") (:dup 2 1))))
              :background nil)
             (parse-shell-line "cmd &>f"))))

(test run-2>&1-merges-stderr-into-stdout
  "`2>&1` sends stderr to wherever stdout goes — so the object stream (which reads
the tail's stdout) sees both lines."
  (is (equal '("OUT" "ERR")
             (shell-eval "sh -c 'echo OUT; echo ERR >&2' 2>&1"))))

(test run-both-to-file-and-order-matters
  (let ((dir (make-temp-dir)))
    (let ((*current-directory* dir))
      ;; &>both.txt : both stdout and stderr into the file
      (shell-eval "sh -c 'echo O; echo E >&2' &> both.txt")
      (is (equal '("E" "O")                       ; both lines present (sorted)
                 (sort (with-open-file (s (merge-pathnames "both.txt" dir))
                         (loop for l = (read-line s nil nil) while l collect l))
                       #'string<)))
      ;; `>f 2>&1` also sends both to the file...
      (shell-eval "sh -c 'echo O; echo E >&2' > ord.txt 2>&1")
      (is (= 2 (length (with-open-file (s (merge-pathnames "ord.txt" dir))
                         (loop for l = (read-line s nil nil) while l collect l)))))
      ;; ...but `2>&1 >f` sends ONLY stdout to the file (stderr stays on the
      ;; inherited fd 2), so the file has just one line
      (shell-eval "sh -c 'echo O; echo E >&2' 2>&1 > only.txt")
      (is (equal '("O")
                 (with-open-file (s (merge-pathnames "only.txt" dir))
                   (loop for l = (read-line s nil nil) while l collect l)))))))
