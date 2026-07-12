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
