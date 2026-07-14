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
