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

(test eval-dollar-escape-evaluates-lisp
  (is (equal '("HI") (shell-eval "echo $(string-upcase \"hi\")"))))

(test eval-comma-escape-uses-a-lisp-value
  (let ((*package* (find-package :consh/test)))
    (is (equal '("world") (shell-eval "echo ,*ct-word*")))))

(test eval-bare-lisp-form
  (is (= 42 (shell-eval "(+ 40 2)"))))

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
      (is (equal '("keep me") (shell-eval "grep keep < in.txt"))))))

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
