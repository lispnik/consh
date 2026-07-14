;;;; lineedit.lisp — an interactive line editor with Tab completion + history.
;;;;
;;;; The editor MODEL (a LEDIT struct and the operations on it) is pure and unit
;;;; tested: keys mutate the buffer/point/history and return an action.  The
;;;; terminal DRIVER (raw mode via stty, reading keystrokes and redrawing with
;;;; ANSI escapes) is a thin layer used only on an interactive tty; the REPL
;;;; falls back to READ-LINE for pipes and scripts.

(in-package #:consh)

;;; ---------------------------------------------------------------------------
;;; Line history (raw command strings, distinct from the (form . result) *history*)
;;; ---------------------------------------------------------------------------

(defvar *line-history* (make-array 0 :adjustable t :fill-pointer 0)
  "Vector of the raw command lines entered, oldest first.")

(defun record-line (line)
  "Add LINE to *line-history* unless blank or a duplicate of the last entry."
  (let ((trimmed (string-trim '(#\Space #\Tab) line)))
    (when (and (plusp (length trimmed))
               (or (zerop (fill-pointer *line-history*))
                   (not (string= trimmed (aref *line-history*
                                               (1- (fill-pointer *line-history*)))))))
      (vector-push-extend trimmed *line-history*)))
  line)

;;; ---------------------------------------------------------------------------
;;; The editor model
;;; ---------------------------------------------------------------------------

(defstruct (ledit (:constructor %make-ledit))
  (text "" :type string)                 ; current line
  (point 0 :type fixnum)                 ; cursor index into TEXT
  (history #() :type vector)             ; command strings, oldest first
  (hidx nil)                             ; NIL editing a fresh line, else history index
  (stash "" :type string))               ; the fresh line stashed while browsing history

(defun make-ledit (&optional (history *line-history*))
  (%make-ledit :history (coerce history 'vector)))

(defun ledit-insert (ed ch)
  (let ((p (ledit-point ed)))
    (setf (ledit-text ed) (concatenate 'string (subseq (ledit-text ed) 0 p)
                                       (string ch) (subseq (ledit-text ed) p))
          (ledit-point ed) (1+ p))))

(defun ledit-backspace (ed)
  (let ((p (ledit-point ed)))
    (when (plusp p)
      (setf (ledit-text ed) (concatenate 'string (subseq (ledit-text ed) 0 (1- p))
                                         (subseq (ledit-text ed) p))
            (ledit-point ed) (1- p)))))

(defun ledit-delete (ed)
  (let ((p (ledit-point ed)))
    (when (< p (length (ledit-text ed)))
      (setf (ledit-text ed) (concatenate 'string (subseq (ledit-text ed) 0 p)
                                         (subseq (ledit-text ed) (1+ p)))))))

(defun ledit-left  (ed) (when (plusp (ledit-point ed)) (decf (ledit-point ed))))
(defun ledit-right (ed) (when (< (ledit-point ed) (length (ledit-text ed))) (incf (ledit-point ed))))
(defun ledit-home  (ed) (setf (ledit-point ed) 0))
(defun ledit-end   (ed) (setf (ledit-point ed) (length (ledit-text ed))))
(defun ledit-kill-to-end (ed) (setf (ledit-text ed) (subseq (ledit-text ed) 0 (ledit-point ed))))
(defun ledit-kill-line (ed) (setf (ledit-text ed) "" (ledit-point ed) 0))

(defun %ledit-set-line (ed line)
  (setf (ledit-text ed) line (ledit-point ed) (length line)))

(defun ledit-history-prev (ed)
  "Step back into older history (Up)."
  (let ((h (ledit-history ed)))
    (when (plusp (length h))
      (cond ((null (ledit-hidx ed))
             (setf (ledit-stash ed) (ledit-text ed)
                   (ledit-hidx ed) (1- (length h))))
            ((plusp (ledit-hidx ed)) (decf (ledit-hidx ed))))
      (%ledit-set-line ed (aref h (ledit-hidx ed))))))

(defun ledit-history-next (ed)
  "Step forward toward newer history (Down); past the newest, restore the stash."
  (let ((h (ledit-history ed)))
    (when (ledit-hidx ed)
      (if (< (ledit-hidx ed) (1- (length h)))
          (progn (incf (ledit-hidx ed)) (%ledit-set-line ed (aref h (ledit-hidx ed))))
          (progn (setf (ledit-hidx ed) nil) (%ledit-set-line ed (ledit-stash ed)))))))

;;; --- Tab completion -------------------------------------------------------

(defun %common-prefix (strings)
  (if (null strings)
      ""
      (reduce (lambda (a b)
                (let ((n (min (length a) (length b))))
                  (subseq a 0 (or (mismatch a b :end1 n :end2 n) n))))
              strings)))

(defun %token-start (text point)
  "Index where the token ending at POINT begins (after the last unquoted
separator)."
  (1+ (or (position-if (lambda (c) (member c '(#\Space #\Tab #\|)))
                       text :end point :from-end t)
          -1)))

(defun ledit-complete (ed)
  "Tab: complete the token before point.  Returns :redraw, or (:show . LIST) when
several candidates remain (the driver lists them)."
  (let* ((text (ledit-text ed)) (point (ledit-point ed))
         (start (%token-start text point))
         (token (subseq text start point))
         (comps (complete-line (subseq text 0 point))))
    (flet ((replace-token (with move-extra)
             (setf (ledit-text ed) (concatenate 'string (subseq text 0 start) with
                                                (subseq text point))
                   (ledit-point ed) (+ start (length with) move-extra))))
      (cond
        ((null comps) :redraw)
        ((= 1 (length comps)) (replace-token (concatenate 'string (first comps) " ") 0) :redraw)
        (t (let ((lcp (%common-prefix comps)))
             (when (> (length lcp) (length token)) (replace-token lcp 0)))
           (cons :show comps))))))

;;; --- Key dispatch (the driver feeds keys here) ----------------------------

(defun ledit-key (ed key)
  "Apply KEY to ED, returning an action: :redraw, :submit, :cancel, :eof, or
(:show . completions).  KEY is a character (insert) or a keyword."
  (if (characterp key)
      (progn (ledit-insert ed key) :redraw)
      (ecase key
        (:backspace   (ledit-backspace ed) :redraw)
        (:delete      (ledit-delete ed) :redraw)
        (:left        (ledit-left ed) :redraw)
        (:right       (ledit-right ed) :redraw)
        (:home        (ledit-home ed) :redraw)
        (:end         (ledit-end ed) :redraw)
        (:kill-to-end (ledit-kill-to-end ed) :redraw)
        (:kill-line   (ledit-kill-line ed) :redraw)
        (:prev        (ledit-history-prev ed) :redraw)
        (:next        (ledit-history-next ed) :redraw)
        (:tab         (ledit-complete ed))
        (:enter       :submit)
        (:cancel      :cancel)
        (:eof         (if (zerop (length (ledit-text ed))) :eof :redraw)))))

;;; ---------------------------------------------------------------------------
;;; Terminal driver (raw mode via stty; interactive tty only)
;;; ---------------------------------------------------------------------------

(defun interactive-terminal-p (&optional (stream *standard-input*))
  "True if STREAM is an interactive terminal (so line editing is appropriate).
NIL for a pipe or file — the REPL then uses plain READ-LINE.  Confirmed with
isatty(3) when the fd is available."
  (and (interactive-stream-p stream)
       (ignore-errors
        (= 1 (cffi:foreign-funcall "isatty"
                                   :int (sb-sys:fd-stream-fd stream) :int)))))

(defun %stty (args)
  "Run stty ARGS on the controlling terminal, waiting for it."
  (ignore-errors (wait-process (launch "stty" args) :timeout 2)))

(defun %read-key (in)
  "Read one logical key from raw-mode stream IN: a character, or a keyword for
control/navigation keys.  Decodes the common CSI arrow/Home/End escapes."
  (let ((c (read-char in nil nil)))
    (cond
      ((null c) :eof)
      ((char= c #\Return) :enter)
      ((char= c #\Newline) :enter)
      ((char= c #\Rubout) :backspace)            ; DEL (0x7f)
      ((char= c #\Backspace) :backspace)         ; ^H
      ((char= c (code-char 3)) :cancel)          ; ^C
      ((char= c (code-char 4)) :eof)             ; ^D
      ((char= c (code-char 1)) :home)            ; ^A
      ((char= c (code-char 5)) :end)             ; ^E
      ((char= c (code-char 11)) :kill-to-end)    ; ^K
      ((char= c (code-char 21)) :kill-line)      ; ^U
      ((char= c #\Tab) :tab)
      ((char= c #\Escape)
       (if (eql (read-char in nil nil) #\[)
           (case (read-char in nil nil)
             (#\A :prev) (#\B :next) (#\C :right) (#\D :left)
             (#\H :home) (#\F :end)
             (#\3 (read-char in nil nil) :delete)  ; ESC[3~
             (t :redraw))
           :redraw))
      (t c))))

(defun %redraw (ed prompt out)
  "Repaint the prompt + line and place the cursor, using ANSI escapes."
  (format out "~C[2K~C~A~A" #\Escape #\Return prompt (ledit-text ed))   ; clear line, home, prompt+text
  (format out "~C[~DG" #\Escape (+ 1 (length prompt) (ledit-point ed))) ; column = prompt+point (1-based)
  (finish-output out))

(defun read-line-edited (prompt &key (in *standard-input*) (out *standard-output*))
  "Read a line with editing/completion/history from raw-mode terminal IN.
Returns the line string, or NIL on EOF."
  (let ((ed (make-ledit)))
    (%stty '("-echo" "-icanon" "min" "1" "time" "0"))
    (unwind-protect
         (progn
           (%redraw ed prompt out)
           (loop
             (let ((action (ledit-key ed (%read-key in))))
               (case action
                 (:submit (format out "~%") (return (ledit-text ed)))
                 (:cancel (format out "^C~%") (%ledit-set-line ed "")
                          (setf (ledit-hidx ed) nil))
                 (:eof (format out "~%") (return nil))
                 (t (when (and (consp action) (eq (car action) :show))
                      (format out "~%~{~A~^  ~}~%" (cdr action)))
                    (%redraw ed prompt out))))))
      (%stty '("sane")))))

;;; ---------------------------------------------------------------------------
;;; The REPL
;;; ---------------------------------------------------------------------------

(defun %present (result out)
  (if (listp result)
      (dolist (x result) (format out "~&~A~%" x))
      (format out "~&~S~%" result)))

(defun %read-repl-line (prompt interactive in out)
  (if interactive
      (read-line-edited prompt :in in :out out)
      (progn (write-string prompt out) (finish-output out) (read-line in nil nil))))

(defun shell-repl (&key (in *standard-input*) (out *standard-output*))
  "Read-eval-print loop over surface syntax.  On an interactive tty it uses the
line editor (Tab completion, Up/Down history, Emacs-ish keys); otherwise plain
READ-LINE.  Reports pending job events before each prompt.  Ctrl-C aborts the
line (or, mid-command, tears the job down); Ctrl-D / EOF ends the loop."
  (let ((interactive (interactive-terminal-p in)))
    (loop
      (dolist (event (take-job-events)) (format out "~&[~A]~%" event))
      (let ((line (handler-case (%read-repl-line (prompt) interactive in out)
                    (sb-sys:interactive-interrupt () (format out "~&^C~%") ""))))
        (when (null line) (return))
        (unless (zerop (length (string-trim '(#\Space #\Tab) line)))
          (record-line line)
          (handler-case (%present (shell-eval line) out)
            (shell-exit (c) (return (shell-exit-code c)))
            (sb-sys:interactive-interrupt () (format out "~&^C~%"))
            (error (e) (format out "~&Error: ~A~%" e))))))))

(defun main ()
  "Entry point for a dumped consh executable: greet, run the REPL, exit cleanly."
  ;; A saved image baked in *current-directory* at build time; adopt the real
  ;; working directory the executable was launched from.
  (ignore-errors
   (setf *current-directory* (truename (pathname (format nil "~A/" (sb-posix:getcwd))))))
  (format t "consh — a Common Lisp Unix shell (objects, not bytes). Ctrl-D to exit.~%")
  (finish-output)
  ;; Load ~/.config/consh/consh.lisp (aliases, prompt, wrappers) — a broken init
  ;; file reports and is swallowed, never blocking startup.
  (when *load-init-file* (load-init-file))
  ;; Take the controlling terminal (if stdin is a tty) so fg/bg can hand it to
  ;; jobs — a no-op under a pipe or without a tty.
  (enable-terminal-job-control 0)
  ;; Read/eval Lisp lines in the CONSH package so the shell's own vocabulary
  ;; (pipe, pipeline-collect, file-size, ...) is available unqualified at the
  ;; prompt — an object shell is only usable if its objects are in reach.
  (unwind-protect
       (handler-case (let ((*package* (find-package '#:consh))) (shell-repl))
         (sb-sys:interactive-interrupt () (terpri)))
    (disable-terminal-job-control))
  (finish-output)
  (sb-ext:exit :code 0))
