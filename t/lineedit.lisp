;;;; t/lineedit.lisp — Phase 6: the interactive line editor model.

(in-package #:consh/test)

(def-suite lineedit :in consh :description "Line editor model: editing, history, completion.")
(in-suite lineedit)

(defun type-string (ed string)
  "Type each character of STRING into ED."
  (loop for c across string do (ledit-key ed c)))

;;; ===========================================================================
;;; Editing
;;; ===========================================================================

(test insert-and-point
  (let ((ed (make-ledit)))
    (type-string ed "abc")
    (is (string= "abc" (ledit-text ed)))
    (is (= 3 (ledit-point ed)))))

(test cursor-movement-and-insert-in-middle
  (let ((ed (make-ledit)))
    (type-string ed "ac")
    (ledit-key ed :left)                       ; between a and c
    (ledit-key ed #\b)
    (is (string= "abc" (ledit-text ed)))
    (is (= 2 (ledit-point ed)))
    (ledit-key ed :home) (is (= 0 (ledit-point ed)))
    (ledit-key ed :end)  (is (= 3 (ledit-point ed)))))

(test highlight-adds-no-visible-width
  "The critical property: highlighting inserts only zero-width escapes, so the
line editor's cursor math (which counts raw characters) stays correct."
  (let ((consh::*highlight* t))
    (dolist (line '("ls | grep foo" "cd \"my dir\" && pwd" "echo $HOME > out"))
      (is (= (length line) (consh::%display-width (consh::%highlight line)))))))

(test highlight-colors-command-validity
  (let ((consh::*highlight* t))
    ;; a known command (builtin) is green, an unknown one red
    (is (search (format nil "~C[32m" #\Escape) (consh::%highlight "cd")))
    (is (search (format nil "~C[31m" #\Escape) (consh::%highlight "no-such-cmd-zzz")))
    ;; a string is yellow, an operator cyan
    (is (search (format nil "~C[33m" #\Escape) (consh::%highlight "echo \"hi\"")))
    (is (search (format nil "~C[36m" #\Escape) (consh::%highlight "a | b")))))

(test highlight-off-returns-raw-text
  (let ((consh::*highlight* nil))
    (is (string= "ls | grep" (consh::%highlight "ls | grep")))))

(test display-width-ignores-ansi-escapes
  "%DISPLAY-WIDTH counts visible columns, skipping ANSI CSI (colour) sequences —
   this is what keeps a coloured prompt from misplacing the cursor."
  (is (= 3 (consh::%display-width "abc")))
  (is (= 0 (consh::%display-width "")))
  ;; a colourised string measures as its visible text only
  (is (= 3 (consh::%display-width (colorize "abc" :red))))
  (is (= 1 (consh::%display-width (colorize "X" :green t))))
  ;; multiple embedded sequences
  (is (= 6 (consh::%display-width
            (concatenate 'string (colorize "foo" :blue) (colorize "bar" :red))))))

(test char-width-handles-wide-and-combining
  (is (= 1 (consh::%char-width #\a)))
  (is (= 2 (consh::%char-width (code-char #x4e16))))    ; 世 East-Asian wide
  (is (= 0 (consh::%char-width (code-char #x0301))))    ; combining acute accent
  (is (= 2 (consh::%char-width (code-char #x1F600))))   ; emoji (wide)
  (is (= 0 (consh::%char-width (code-char 7)))))        ; control char (bell)

(test display-width-counts-columns-not-characters
  (is (= 4 (consh::%display-width "a世b")))              ; 1 + 2 + 1 columns
  (is (= 3 (consh::%display-width "a世b" 0 2)))          ; range: a + 世
  ;; a base letter plus a combining mark is one column, not two
  (is (= 1 (consh::%display-width (coerce (list #\e (code-char #x0301)) 'string))))
  ;; ASCII width is still the character count
  (is (= 9 (consh::%display-width "ls | grep"))))

(test backspace-and-delete
  (let ((ed (make-ledit)))
    (type-string ed "abcd")
    (ledit-key ed :backspace)                  ; -> "abc"
    (is (string= "abc" (ledit-text ed)))
    (ledit-key ed :home) (ledit-key ed :delete) ; delete 'a' -> "bc"
    (is (string= "bc" (ledit-text ed)))
    (is (= 0 (ledit-point ed)))))

(test kill-operations
  (let ((ed (make-ledit)))
    (type-string ed "hello world")
    (ledit-key ed :home) (dotimes (i 5) (ledit-key ed :right))  ; point after "hello"
    (ledit-key ed :kill-to-end)
    (is (string= "hello" (ledit-text ed)))
    (ledit-key ed :kill-line)
    (is (string= "" (ledit-text ed)))
    (is (= 0 (ledit-point ed)))))

(test word-motion-moves-by-words
  (let ((ed (make-ledit)))
    (type-string ed "foo bar baz")               ; point at end (11)
    (ledit-key ed :back-word) (is (= 8 (ledit-point ed)))   ; start of "baz"
    (ledit-key ed :back-word) (is (= 4 (ledit-point ed)))   ; start of "bar"
    (ledit-key ed :forward-word) (is (= 7 (ledit-point ed))) ; end of "bar"
    (ledit-key ed :home)
    (ledit-key ed :forward-word) (is (= 3 (ledit-point ed))))) ; end of "foo"

(test kill-word-back-and-forward
  (let ((consh:*kill-ring* '()))
    (let ((ed (make-ledit)))
      (type-string ed "alpha beta gamma")         ; point at end
      (ledit-key ed :kill-word-back)              ; removes "gamma"
      (is (string= "alpha beta " (ledit-text ed)))
      (is (string= "gamma" (first consh:*kill-ring*)))
      (ledit-key ed :home)
      (ledit-key ed :kill-word-forward)           ; removes "alpha"
      (is (string= " beta " (ledit-text ed)))
      (is (string= "alpha" (first consh:*kill-ring*))))))

(test yank-reinserts-the-last-kill
  (let ((consh:*kill-ring* '()))
    (let ((ed (make-ledit)))
      (type-string ed "keep this")
      (ledit-key ed :kill-line)                   ; ring gets "keep this"
      (is (string= "" (ledit-text ed)))
      (type-string ed "x ")
      (ledit-key ed :yank)                        ; paste it back at point
      (is (string= "x keep this" (ledit-text ed)))
      (is (= 11 (ledit-point ed))))))

(test kill-to-end-feeds-the-ring
  (let ((consh:*kill-ring* '()))
    (let ((ed (make-ledit)))
      (type-string ed "abcdef")
      (ledit-key ed :home) (dotimes (i 3) (ledit-key ed :right))
      (ledit-key ed :kill-to-end)                 ; kills "def"
      (is (string= "abc" (ledit-text ed)))
      (is (string= "def" (first consh:*kill-ring*))))))

(test transpose-swaps-characters
  (let ((ed (make-ledit)))
    (type-string ed "acb")
    (ledit-key ed :left)                          ; point between c and b (index 2)
    (ledit-key ed :transpose)                     ; swap c and b -> "abc"
    (is (string= "abc" (ledit-text ed)))))

;;; ===========================================================================
;;; Readline parity: undo, ^D delete-char, yank-pop, yank-last-arg, ^F/^B/^P/^N
;;; ===========================================================================

(test undo-reverts-edits
  (let ((ed (make-ledit)))
    (type-string ed "hello")                      ; one insert run -> one undo unit
    (ledit-key ed :kill-word-back)                ; -> ""
    (is (string= "" (ledit-text ed)))
    (ledit-key ed :undo)                          ; back to "hello"
    (is (string= "hello" (ledit-text ed)))
    (ledit-key ed :undo)                          ; back to "" (before the insert run)
    (is (string= "" (ledit-text ed)))))

(test undo-coalesces-an-insert-run-then-steps-back-per-edit
  (let ((ed (make-ledit)))
    (type-string ed "abc")                        ; run 1
    (ledit-key ed :left) (ledit-key ed :backspace) ; a discrete edit -> "ac"? point moved
    (is (string= "ac" (ledit-text ed)))
    (ledit-key ed :undo)                          ; undo the backspace
    (is (string= "abc" (ledit-text ed)))
    (ledit-key ed :undo)                          ; undo the whole insert run
    (is (string= "" (ledit-text ed)))))

(test ctrl-d-deletes-char-midline-but-eofs-when-empty
  (let ((ed (make-ledit)))
    (type-string ed "abc") (ledit-key ed :home)
    (is (eq :redraw (ledit-key ed :eof)))         ; mid-line: delete char under cursor
    (is (string= "bc" (ledit-text ed)))
    (is (eq :redraw (ledit-key ed :eof))) (is (string= "c" (ledit-text ed)))
    (is (eq :redraw (ledit-key ed :eof))) (is (string= "" (ledit-text ed)))
    (is (eq :eof (ledit-key ed :eof)))))          ; empty line: EOF

(test yank-pop-cycles-the-kill-ring
  (let ((consh:*kill-ring* '()))
    (let ((ed (make-ledit)))
      ;; build a ring by killing whole lines: newest first -> ("two" "one")
      (type-string ed "one") (ledit-key ed :kill-line)
      (type-string ed "two") (ledit-key ed :kill-line)
      (is (equal '("two" "one") consh:*kill-ring*))
      (ledit-key ed :yank)                        ; inserts "two"
      (is (string= "two" (ledit-text ed)))
      (ledit-key ed :yank-pop)                    ; replaces with "one"
      (is (string= "one" (ledit-text ed)))
      (ledit-key ed :yank-pop)                    ; cycles back to "two"
      (is (string= "two" (ledit-text ed))))))

(test yank-last-arg-inserts-last-word-of-previous-command
  (let ((ed (make-ledit (vector "git commit -m msg" "cd /var/log"))))
    (type-string ed "ls ")
    (ledit-key ed :yank-last-arg)                 ; last word of newest entry -> "/var/log"
    (is (string= "ls /var/log" (ledit-text ed)))))

(test movement-control-keys-decode
  (is (equal (list :left)  (%keys-from (string (code-char 2)))))   ; ^B
  (is (equal (list :right) (%keys-from (string (code-char 6)))))   ; ^F
  (is (equal (list :prev)  (%keys-from (string (code-char 16)))))  ; ^P
  (is (equal (list :next)  (%keys-from (string (code-char 14)))))  ; ^N
  (is (equal (list :undo)  (%keys-from (string (code-char 31)))))  ; ^_
  (is (equal (list :yank-pop)      (%keys-from (format nil "~Cy" #\Escape))))  ; M-y
  (is (equal (list :yank-last-arg) (%keys-from (format nil "~C." #\Escape))))) ; M-.

;;; ===========================================================================
;;; Reverse incremental search (^R)
;;; ===========================================================================

(test reverse-search-finds-and-cycles-history
  (let ((ed (make-ledit (vector "git commit" "ls -la" "grep foo" "git push"))))
    (ledit-key ed :reverse-search)                ; enter search mode
    (type-string ed "git")                        ; query -> newest match
    (is (string= "git push" (ledit-text ed)))
    (ledit-key ed :reverse-search)                ; step to the older match
    (is (string= "git commit" (ledit-text ed)))
    (is (eq :submit (ledit-key ed :enter)))       ; accept + submit
    (is (string= "git commit" (ledit-text ed)))))

(test reverse-search-cancel-restores-line
  (let ((ed (make-ledit (vector "alpha" "beta"))))
    (type-string ed "orig")                       ; a fresh line in progress
    (ledit-key ed :reverse-search)
    (type-string ed "bet")                        ; matches "beta"
    (is (string= "beta" (ledit-text ed)))
    (ledit-key ed :cancel)                        ; ^C in search restores the line
    (is (string= "orig" (ledit-text ed)))
    (is (= 4 (ledit-point ed)))))

(test reverse-search-movement-key-accepts-match
  (let ((ed (make-ledit (vector "one two" "three four"))))
    (ledit-key ed :reverse-search)
    (type-string ed "three")
    (is (string= "three four" (ledit-text ed)))
    (ledit-key ed :home)                          ; a movement key accepts + acts
    (is (= 0 (ledit-point ed)))
    (type-string ed "X ")                         ; back in ordinary editing
    (is (string= "X three four" (ledit-text ed)))))

(test reverse-search-failed-query-keeps-line
  (let ((ed (make-ledit (vector "abc" "def"))))
    (ledit-key ed :reverse-search)
    (type-string ed "zzz")                        ; no match
    (is-true (consh::ledit-sfailed ed))
    (is (string= "" (ledit-text ed)))))           ; the line is left untouched

(test reverse-search-backspace-widens-query
  (let ((ed (make-ledit (vector "make build" "make test"))))
    (ledit-key ed :reverse-search)
    (type-string ed "test")                       ; matches "make test"
    (is (string= "make test" (ledit-text ed)))
    (ledit-key ed :backspace)                     ; query "tes" — still matches test
    (ledit-key ed :backspace)                     ; "te"
    (is (string= "make test" (ledit-text ed)))))

;;; ===========================================================================
;;; Autosuggestions (fish-style ghost text)
;;; ===========================================================================

(test autosuggestion-offers-newest-history-tail
  (let ((ed (make-ledit (vector "git status" "git commit -m x"))))
    (type-string ed "git c")
    (is (string= "ommit -m x" (consh::%ledit-suggestion ed)))   ; newest matching entry
    (ledit-key ed :right)                                       ; Right accepts it
    (is (string= "git commit -m x" (ledit-text ed)))))

(test autosuggestion-only-at-end-and-on-match
  (let ((ed (make-ledit (vector "hello world"))))
    (type-string ed "hel")
    (is (string= "lo world" (consh::%ledit-suggestion ed)))
    (ledit-key ed :left)                                        ; point no longer at end
    (is (null (consh::%ledit-suggestion ed))))
  (let ((ed (make-ledit (vector "abc"))))                       ; no matching history
    (type-string ed "xyz")
    (is (null (consh::%ledit-suggestion ed)))))

(test autosuggestion-end-key-accepts
  (let ((ed (make-ledit (vector "make prompt-demo"))))
    (type-string ed "make ")
    (ledit-key ed :end)
    (is (string= "make prompt-demo" (ledit-text ed)))))

(test autosuggestion-disabled-by-flag
  (let ((consh::*autosuggest* nil)
        (ed (make-ledit (vector "hello world"))))
    (type-string ed "hel")
    (is (null (consh::%ledit-suggestion ed)))))

(test boundary-moves-are-safe
  (let ((ed (make-ledit)))
    (ledit-key ed :left) (ledit-key ed :backspace) (ledit-key ed :delete) (ledit-key ed :right)
    (is (string= "" (ledit-text ed)))
    (is (= 0 (ledit-point ed)))))

;;; ===========================================================================
;;; History (Up/Down) with a stashed in-progress line
;;; ===========================================================================

(test history-up-down
  (let ((ed (make-ledit #("one" "two" "three"))))
    (ledit-key ed :prev) (is (string= "three" (ledit-text ed)))
    (ledit-key ed :prev) (is (string= "two" (ledit-text ed)))
    (ledit-key ed :prev) (is (string= "one" (ledit-text ed)))
    (ledit-key ed :prev) (is (string= "one" (ledit-text ed)))   ; clamps at oldest
    (ledit-key ed :next) (is (string= "two" (ledit-text ed)))))

(test history-restores-stashed-line
  (let ((ed (make-ledit #("older"))))
    (type-string ed "in progress")
    (ledit-key ed :prev) (is (string= "older" (ledit-text ed)))
    (ledit-key ed :next) (is (string= "in progress" (ledit-text ed)))))  ; stash restored

(test history-empty-is-noop
  (let ((ed (make-ledit #())))
    (ledit-key ed :prev)
    (is (string= "" (ledit-text ed)))))

;;; ===========================================================================
;;; Tab completion
;;; ===========================================================================

(test complete-sole-inserts-and-appends-space
  (let ((dir (make-temp-dir)))
    (with-open-file (s (merge-pathnames "uniquefile" dir) :direction :output)
      (write-string "x" s))
    (let ((*current-directory* dir)
          (ed (make-ledit)))
      (type-string ed "cat uniqu")
      (is (eq :redraw (ledit-key ed :tab)))
      (is (string= "cat uniquefile " (ledit-text ed)))
      (is (= (length "cat uniquefile ") (ledit-point ed))))))

(test complete-many-shows-and-extends-common-prefix
  (let ((dir (make-temp-dir)))
    (dolist (n '("aa1" "aa2")) (with-open-file (s (merge-pathnames n dir) :direction :output)
                                 (write-string "x" s)))
    (let ((*current-directory* dir)
          (ed (make-ledit)))
      (type-string ed "cat a")
      (let ((action (ledit-key ed :tab)))
        (is (consp action))
        (is (eq :show (car action)))
        (is (equal '("aa1" "aa2") (cdr action)))
        ;; the common prefix "aa" was filled in
        (is (string= "cat aa" (ledit-text ed)))))))

(test complete-command-context
  (let ((ed (make-ledit)))
    (type-string ed "pw")
    (ledit-key ed :tab)
    ;; "pwd" is a builtin; whatever the PATH adds, completion keeps the pw prefix
    (is (eql 0 (search "pw" (ledit-text ed))))))

(test tab-cycles-through-candidates
  (let ((dir (make-temp-dir)))
    (dolist (n '("aa1" "aa2"))
      (with-open-file (s (merge-pathnames n dir) :direction :output) (write-string "x" s)))
    (let ((*current-directory* dir) (ed (make-ledit)))
      (type-string ed "cat a")
      ;; first Tab: fills the common prefix and offers the list
      (let ((first (ledit-key ed :tab)))
        (is (consp first))
        (is (eq :show (car first)))
        (is (string= "cat aa" (ledit-text ed))))
      ;; subsequent Tabs cycle through the candidates and wrap
      (ledit-key ed :tab) (is (string= "cat aa1" (ledit-text ed)))
      (ledit-key ed :tab) (is (string= "cat aa2" (ledit-text ed)))
      (ledit-key ed :tab) (is (string= "cat aa1" (ledit-text ed)))
      ;; typing breaks the cycle: a fresh Tab context
      (type-string ed "!") (is (string= "cat aa1!" (ledit-text ed))))))

;;; ===========================================================================
;;; Key dispatch actions and helpers
;;; ===========================================================================

(test key-dispatch-actions
  (let ((ed (make-ledit)))
    (is (eq :submit (ledit-key ed :enter)))
    (is (eq :cancel (ledit-key ed :cancel)))
    (is (eq :eof (ledit-key ed :eof)))          ; empty line -> EOF
    (type-string ed "x")
    (is (eq :redraw (ledit-key ed :eof)))))     ; non-empty -> not EOF

(test record-line-dedups-and-skips-blank
  (let ((*line-history* (make-array 0 :adjustable t :fill-pointer 0)))
    (record-line "a")
    (record-line "a")                            ; duplicate of last -> skipped
    (record-line "   ")                          ; blank -> skipped
    (record-line "b")
    (is (equalp #("a" "b") *line-history*))))

(test interactive-terminal-p-false-for-non-tty
  (is (null (interactive-terminal-p (make-string-input-stream "hi")))))

(defmacro with-temp-history ((path-var) &body body)
  "Run BODY with a throwaway history file bound as *history-file* and persistence
enabled; delete the file afterward."
  `(let* ((,path-var (merge-pathnames (format nil "consh-hist-~D.txt" (sb-posix:getpid))
                                      #P"/tmp/"))
          (consh:*history-file* ,path-var)
          (consh:*history-persist* t)
          (consh:*line-history* (make-array 0 :adjustable t :fill-pointer 0)))
     (unwind-protect (progn ,@body)
       (ignore-errors (delete-file ,path-var)))))

(test history-persists-across-sessions
  (with-temp-history (path)
    ;; "session one": record a few lines; they append to the file
    (record-line "ls")
    (record-line "grep foo")
    (record-line "grep foo")                       ; dup of last -> not re-recorded
    (record-line "cd src")
    (is (probe-file path))
    ;; "session two": a fresh in-memory history reloads from disk
    (let ((consh:*line-history* (make-array 0 :adjustable t :fill-pointer 0)))
      (load-history-file)
      (is (equalp #("ls" "grep foo" "cd src") consh:*line-history*)))))

(test history-not-written-when-persistence-off
  (let* ((path (merge-pathnames (format nil "consh-nopersist-~D.txt" (sb-posix:getpid))
                                #P"/tmp/"))
         (consh:*history-file* path)
         (consh:*history-persist* nil)             ; the default: memory only
         (consh:*line-history* (make-array 0 :adjustable t :fill-pointer 0)))
    (unwind-protect
         (progn (record-line "secret command")
                (is (equalp #("secret command") consh:*line-history*))  ; in memory
                (is (null (probe-file path))))                          ; but not on disk
      (ignore-errors (delete-file path)))))

(test history-file-compacts-past-the-cap
  (with-temp-history (path)
    (let ((consh:*history-max* 3))
      ;; write 5 distinct lines to the file directly
      (with-open-file (s path :direction :output :if-exists :supersede
                              :if-does-not-exist :create)
        (dolist (l '("one" "two" "three" "four" "five")) (write-line l s)))
      (load-history-file)
      ;; only the last 3 survive, in memory and (compacted) on disk
      (is (equalp #("three" "four" "five") consh:*line-history*))
      (let ((on-disk (with-open-file (s path)
                       (loop for l = (read-line s nil nil) while l collect l))))
        (is (equal '("three" "four" "five") on-disk))))))

;;; --- driver hardening ------------------------------------------------------

(test unknown-key-is-a-harmless-redraw-not-a-crash
  "ledit-key must tolerate :redraw (which %read-key returns for unrecognized
escapes) and any other keyword — a repaint, not a CASE-FAILURE."
  (let ((ed (make-ledit)))
    (is (eq :redraw (ledit-key ed :redraw)))
    (is (eq :redraw (ledit-key ed :some-future-key)))
    (is (string= "" (ledit-text ed)))))            ; buffer untouched

(defun %keys-from (string)
  "Read all logical keys from STRING via %read-key until :eof."
  (with-input-from-string (s string)
    (loop for k = (consh::%read-key s) until (eq k :eof) collect k)))

(test bracketed-paste-inserts-as-one-edit
  (let ((ed (make-ledit)))
    (type-string ed "x ")
    (ledit-key ed (cons :paste "foo bar"))         ; a paste key inserts the whole payload
    (is (string= "x foo bar" (ledit-text ed)))
    (is (= 9 (ledit-point ed)))
    (ledit-key ed :undo)                            ; and it's a single undo unit
    (is (string= "x " (ledit-text ed)))))

(test bracketed-paste-decodes-and-flattens-newlines
  "ESC[200~ … ESC[201~ decodes to one (:paste . text) key with newlines flattened
to spaces, so a multi-line paste never runs a command per line."
  (flet ((esc (s) (concatenate 'string (string #\Escape) s)))
    ;; ESC[200~ a <newline> b ESC[201~  ->  (:paste . "a b")
    (is (equal (list (cons :paste "a b"))
               (%keys-from (concatenate 'string (esc "[200~") "a" (string #\Newline) "b"
                                        (esc "[201~")))))
    ;; text after the paste terminator is read normally
    (is (equal (list (cons :paste "hi") #\z)
               (%keys-from (concatenate 'string (esc "[200~") "hi" (esc "[201~") "z"))))))

(test csi-sequence-is-fully-consumed-no-tail-leak
  "A full CSI (e.g. ctrl-right ESC[1;5C) maps to one key and its parameter tail
does NOT leak as literal inserts."
  (is (equal (list :right #\x)
             (%keys-from (format nil "~C[1;5Cx" #\Escape))))
  ;; the plain arrows still work
  (is (equal (list :prev :next :left) (%keys-from (format nil "~C[A~C[B~C[D" #\Escape #\Escape #\Escape))))
  ;; ESC[3~ is Delete; the trailing ~ is consumed
  (is (equal (list :delete #\q) (%keys-from (format nil "~C[3~Cq" #\Escape #\~)))))

(test unhandled-escape-and-control-chars-are-ignored
  "PageUp (ESC[5~) and a stray control char (^G) are ignored (:redraw), and the
following printable char is read normally."
  (is (equal (list :redraw #\y) (%keys-from (format nil "~C[5~Cy" #\Escape #\~))))
  (is (equal (list :redraw #\z) (%keys-from (format nil "~Cz" (code-char 7)))))   ; ^G
  (is (equal (list #\a) (%keys-from "a"))))        ; a printable char still inserts

(test emacs-control-and-meta-keys-decode
  "The editing keys added for word-wise editing decode from their bytes."
  (is (equal (list :kill-word-back)    (%keys-from (string (code-char 23)))))  ; ^W
  (is (equal (list :yank)              (%keys-from (string (code-char 25)))))  ; ^Y
  (is (equal (list :clear)             (%keys-from (string (code-char 12)))))  ; ^L
  (is (equal (list :transpose)         (%keys-from (string (code-char 20)))))  ; ^T
  ;; Meta (ESC-prefixed) word keys
  (is (equal (list :back-word)         (%keys-from (format nil "~Cb" #\Escape))))
  (is (equal (list :forward-word)      (%keys-from (format nil "~Cf" #\Escape))))
  (is (equal (list :kill-word-forward) (%keys-from (format nil "~Cd" #\Escape)))))

(test save-termios-is-nil-for-a-non-terminal
  "save-termios on a pipe fd yields NIL (not a terminal), and restore-termios of
NIL is a safe no-op."
  (multiple-value-bind (r w) (make-pipe)
    (unwind-protect
         (progn (is (null (save-termios r)))
                (is (null (restore-termios r nil))))
      (c-close r) (c-close w))))

(test terminal-fd-of-unwraps-wrapper-streams
  "%terminal-fd-of finds the fd through the SYNONYM-/TWO-WAY-STREAM wrappers that
*standard-input* is — a plain (sb-sys:fd-stream-fd) errors on those, which is why
the line editor used to be disabled in the dumped image."
  (multiple-value-bind (r w) (make-pipe)
    (let ((fds (sb-sys:make-fd-stream r :input t)))
      (unwind-protect
           (progn
             (is (eql r (consh::%terminal-fd-of fds)))                ; direct fd-stream
             (is (eql r (consh::%terminal-fd-of                       ; through two-way
                         (make-two-way-stream fds (make-broadcast-stream)))))
             (let ((consh::*fd-of-test-target* fds))
               (declare (special consh::*fd-of-test-target*))
               (is (eql r (consh::%terminal-fd-of                     ; through synonym
                           (make-synonym-stream 'consh::*fd-of-test-target*))))))
        (ignore-errors (close fds))
        (ignore-errors (c-close w))))))
