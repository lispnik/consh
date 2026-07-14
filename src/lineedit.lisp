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

(defvar *history-file* :default
  "Where the line history persists across sessions.  :DEFAULT means
<config>/consh/history (alongside consh.lisp); a pathname overrides it; NIL puts
history in memory only.")

(defparameter *history-max* 1000
  "Cap on retained history entries, in memory and on disk.")

(defvar *history-persist* nil
  "When true, RECORD-LINE also appends to the history file.  The dumped entry
point turns this on after loading; it stays off in-process (and in tests) so
programmatic use never writes to the user's history file.")

(defun history-file-path ()
  "Pathname of the persistent history file, or NIL when persistence is disabled."
  (case *history-file*
    ((nil) nil)
    (:default (merge-pathnames "consh/history" (%config-home)))
    (t *history-file*)))

(defun %history-add (line)
  "Add LINE to *line-history* unless blank or a duplicate of the last entry.
Returns the trimmed line when it was added, else NIL."
  (let ((trimmed (string-trim '(#\Space #\Tab) line)))
    (when (and (plusp (length trimmed))
               (or (zerop (fill-pointer *line-history*))
                   (not (string= trimmed (aref *line-history*
                                               (1- (fill-pointer *line-history*)))))))
      (vector-push-extend trimmed *line-history*)
      trimmed)))

(defun %append-history-line (line)
  "Best-effort append of LINE to the history file (errors are swallowed)."
  (let ((path (history-file-path)))
    (when path
      (ignore-errors
        (ensure-directories-exist path)
        (with-open-file (s path :direction :output :if-exists :append
                                :if-does-not-exist :create :external-format :utf-8)
          (write-line line s))))))

(defun record-line (line)
  "Add LINE to *line-history* (blank/duplicate-filtered); when persistence is on,
also append the new entry to the history file."
  (let ((added (%history-add line)))
    (when (and added *history-persist*)
      (%append-history-line added)))
  line)

(defun %rewrite-history-file (path lines)
  "Overwrite PATH with LINES (best-effort) — used to compact an overgrown file."
  (ignore-errors
    (ensure-directories-exist path)
    (with-open-file (s path :direction :output :if-exists :supersede
                            :if-does-not-exist :create :external-format :utf-8)
      (dolist (l lines) (write-line l s)))))

(defun load-history-file (&key (path (history-file-path)))
  "Populate *line-history* from PATH (oldest first), keeping the last *history-max*
entries; compact the file if it had grown past the cap.  Best-effort — a missing
or unreadable file is ignored.  Returns the resulting entry count."
  (let ((file (and path (probe-file path))))
    (when file
      (ignore-errors
        (let* ((lines (with-open-file (s file :external-format :utf-8)
                        (loop for l = (read-line s nil nil) while l collect l)))
               (tail  (last lines *history-max*)))
          (dolist (l tail) (%history-add l))
          (when (> (length lines) *history-max*)
            (%rewrite-history-file path tail)))))
    (fill-pointer *line-history*)))

;;; ---------------------------------------------------------------------------
;;; The editor model
;;; ---------------------------------------------------------------------------

(defstruct (ledit (:constructor %make-ledit))
  (text "" :type string)                 ; current line
  (point 0 :type fixnum)                 ; cursor index into TEXT
  (history #() :type vector)             ; command strings, oldest first
  (hidx nil)                             ; NIL editing a fresh line, else history index
  (stash "" :type string)                ; the fresh line stashed while browsing history
  ;; reverse-incremental-search (^R) sub-mode state
  (searching nil)                        ; T while in ^R search mode
  (squery "" :type string)               ; the accumulated search query
  (sindex nil)                           ; history index of the current match
  (sfailed nil)                          ; T when the query matches nothing
  (sorig-text "" :type string)           ; line to restore if the search is cancelled
  (sorig-point 0 :type fixnum)
  ;; Tab-completion cycling state (repeated Tab cycles the candidate list)
  (comp-list nil)                        ; candidates remembered from the last Tab
  (comp-idx nil)                         ; NIL = showing common prefix, else cycle index
  (comp-start 0 :type fixnum)            ; token start of the last completion
  (comp-point 0 :type fixnum)            ; point right after the last completion
  (comp-token "" :type string))          ; token text left by the last completion

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
;;; --- kill-ring + word-wise editing ---------------------------------------

(defvar *kill-ring* '()
  "Recently killed text, most-recent first; ^Y (yank) inserts the head.  Kills
from ^K/^U/^W/M-d push here.  Shared across lines so text killed on one line can
be yanked on the next.")

(defparameter *kill-ring-max* 60 "Cap on retained kill-ring entries.")

(defun %kill-record (string)
  "Push non-empty STRING onto *KILL-RING*, trimming to *KILL-RING-MAX*."
  (when (plusp (length string))
    (push string *kill-ring*)
    (when (> (length *kill-ring*) *kill-ring-max*)
      (setf *kill-ring* (subseq *kill-ring* 0 *kill-ring-max*)))))

(defun ledit-kill-to-end (ed)
  "^K: kill from point to end of line."
  (%kill-record (subseq (ledit-text ed) (ledit-point ed)))
  (setf (ledit-text ed) (subseq (ledit-text ed) 0 (ledit-point ed))))

(defun ledit-kill-line (ed)
  "^U: kill the whole line."
  (%kill-record (ledit-text ed))
  (setf (ledit-text ed) "" (ledit-point ed) 0))

(defun ledit-yank (ed)
  "^Y: insert the most recent kill at point."
  (let ((s (first *kill-ring*)))
    (when s
      (let ((p (ledit-point ed)))
        (setf (ledit-text ed) (concatenate 'string (subseq (ledit-text ed) 0 p)
                                           s (subseq (ledit-text ed) p))
              (ledit-point ed) (+ p (length s)))))))

(defun %word-char-p (c) (alphanumericp c))

(defun %forward-word-index (text point)
  "Index after the word at/after POINT: skip separators, then word chars."
  (let ((n (length text)) (i point))
    (loop while (and (< i n) (not (%word-char-p (char text i)))) do (incf i))
    (loop while (and (< i n) (%word-char-p (char text i))) do (incf i))
    i))

(defun %backward-word-index (text point)
  "Index at the start of the word before POINT: skip separators back, then word
chars back."
  (let ((i point))
    (loop while (and (> i 0) (not (%word-char-p (char text (1- i))))) do (decf i))
    (loop while (and (> i 0) (%word-char-p (char text (1- i)))) do (decf i))
    i))

(defun ledit-forward-word (ed)      ; M-f
  (setf (ledit-point ed) (%forward-word-index (ledit-text ed) (ledit-point ed))))

(defun ledit-backward-word (ed)     ; M-b
  (setf (ledit-point ed) (%backward-word-index (ledit-text ed) (ledit-point ed))))

(defun ledit-kill-word-forward (ed) ; M-d
  (let* ((text (ledit-text ed)) (p (ledit-point ed))
         (end (%forward-word-index text p)))
    (when (> end p)
      (%kill-record (subseq text p end))
      (setf (ledit-text ed) (concatenate 'string (subseq text 0 p) (subseq text end))))))

(defun ledit-kill-word-back (ed)    ; ^W / M-DEL
  (let* ((text (ledit-text ed)) (p (ledit-point ed))
         (start (%backward-word-index text p)))
    (when (< start p)
      (%kill-record (subseq text start p))
      (setf (ledit-text ed) (concatenate 'string (subseq text 0 start) (subseq text p))
            (ledit-point ed) start))))

(defun ledit-transpose (ed)         ; ^T: swap the two chars around point
  (let* ((text (copy-seq (ledit-text ed))) (n (length text)) (p (ledit-point ed)))
    (when (>= n 2)
      (let ((i (if (< p n) p (1- p))))       ; index of the right-hand char to swap
        (when (>= i 1)
          (rotatef (char text (1- i)) (char text i))
          (setf (ledit-text ed) text
                (ledit-point ed) (min n (1+ i))))))))

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

;;; --- autosuggestions (fish-style ghost text) -----------------------------

(defvar *autosuggest* t
  "When true, the line editor shows a dim suggestion — the tail of the most
recent history entry that extends what you've typed — which Right/End accepts.")

(defun %ledit-suggestion (ed)
  "The autosuggestion suffix for ED: the tail of the newest history entry that
strictly extends the current text, or NIL.  Only offered when point is at end of
the line and no other sub-mode is active."
  (let ((text (ledit-text ed)))
    (when (and *autosuggest*
               (not (ledit-searching ed))
               (plusp (length text))
               (= (ledit-point ed) (length text)))
      (let ((h (ledit-history ed)))
        (loop for i from (1- (length h)) downto 0
              for cand = (aref h i)
              when (and (> (length cand) (length text))
                        (string= text cand :end2 (length text)))
                return (subseq cand (length text)))))))

(defun ledit-accept-suggestion (ed)
  "If a suggestion is showing, append it and jump to end; return T when it did."
  (let ((s (%ledit-suggestion ed)))
    (when s
      (%ledit-set-line ed (concatenate 'string (ledit-text ed) s))
      t)))

;;; --- reverse incremental search (^R) -------------------------------------

(defun %history-search-backward (history query before-index)
  "Index of the newest history entry at or before BEFORE-INDEX whose text
contains QUERY (case-sensitive substring), or NIL.  Empty QUERY matches any
entry, so it walks the whole history."
  (loop for i from (min before-index (1- (length history))) downto 0
        when (search query (aref history i)) return i))

(defun %ledit-search-from (ed start)
  "Search backward from index START for the current query; on a hit, adopt that
history line; on a miss, keep the line but mark the search failed."
  (let ((i (%history-search-backward (ledit-history ed) (ledit-squery ed) start)))
    (cond (i (setf (ledit-sindex ed) i
                   (ledit-sfailed ed) nil)
             (%ledit-set-line ed (aref (ledit-history ed) i)))
          (t (setf (ledit-sfailed ed) t)))))

(defun ledit-reverse-search (ed)
  "^R: enter reverse-search mode, or (already in it) step to an older match."
  (let ((h (ledit-history ed)))
    (cond
      ((zerop (length h)) nil)
      ((not (ledit-searching ed))
       ;; enter; leave the line untouched until the user types or hits ^R again
       (setf (ledit-searching ed) t
             (ledit-squery ed) ""
             (ledit-sfailed ed) nil
             (ledit-sindex ed) (length h)          ; first search covers everything
             (ledit-sorig-text ed) (ledit-text ed)
             (ledit-sorig-point ed) (ledit-point ed)))
      (t                                            ; strictly older than the current match
       (%ledit-search-from ed (1- (or (ledit-sindex ed) (length h))))))))

(defun ledit-search-type (ed ch)
  "Extend the search query and re-search from the current match position."
  (setf (ledit-squery ed) (concatenate 'string (ledit-squery ed) (string ch)))
  (%ledit-search-from ed (or (ledit-sindex ed) (length (ledit-history ed)))))

(defun ledit-search-backspace (ed)
  "Shorten the search query and re-search."
  (let ((q (ledit-squery ed)))
    (when (plusp (length q))
      (setf (ledit-squery ed) (subseq q 0 (1- (length q))))
      (%ledit-search-from ed (or (ledit-sindex ed) (length (ledit-history ed)))))))

(defun ledit-search-accept (ed)
  "Leave search mode keeping the matched line as a fresh edit."
  (setf (ledit-searching ed) nil (ledit-hidx ed) nil))

(defun ledit-search-cancel (ed)
  "Leave search mode, restoring the line as it was before ^R."
  (setf (ledit-searching ed) nil (ledit-hidx ed) nil)
  (%ledit-set-line ed (ledit-sorig-text ed))
  (setf (ledit-point ed) (min (ledit-sorig-point ed) (length (ledit-text ed)))))

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

(defun %completion-repeat-p (ed start point token)
  "True when this Tab immediately follows a prior completion at the same spot —
the signal to cycle to the next candidate instead of completing afresh."
  (and (ledit-comp-list ed)
       (eql start (ledit-comp-start ed))
       (eql point (ledit-comp-point ed))
       (string= token (ledit-comp-token ed))))

(defun %set-completion-anchor (ed start point token list idx)
  (setf (ledit-comp-start ed) start (ledit-comp-point ed) point
        (ledit-comp-token ed) token (ledit-comp-list ed) list (ledit-comp-idx ed) idx))

(defun %clear-completion (ed)
  (setf (ledit-comp-list ed) nil (ledit-comp-idx ed) nil))

(defun ledit-complete (ed)
  "Tab: complete the token before point.  A unique candidate is inserted with a
trailing space; several candidates fill in their common prefix and are listed
via (:show . LIST); pressing Tab again then cycles through them."
  (let* ((text (ledit-text ed)) (point (ledit-point ed))
         (start (%token-start text point))
         (token (subseq text start point)))
    (flet ((replace-token (with move-extra)
             (setf (ledit-text ed) (concatenate 'string (subseq text 0 start) with
                                                (subseq text point))
                   (ledit-point ed) (+ start (length with) move-extra))))
      (cond
        ;; a repeat Tab: cycle to the next remembered candidate
        ((%completion-repeat-p ed start point token)
         (let* ((list (ledit-comp-list ed))
                (idx  (if (ledit-comp-idx ed) (mod (1+ (ledit-comp-idx ed)) (length list)) 0))
                (cand (nth idx list)))
           (replace-token cand 0)
           (%set-completion-anchor ed start (+ start (length cand)) cand list idx)
           :redraw))
        ;; a fresh completion
        (t (let ((comps (complete-line (subseq text 0 point))))
             (cond
               ((null comps) (%clear-completion ed) :redraw)
               ((= 1 (length comps))
                (replace-token (concatenate 'string (first comps) " ") 0)
                (%clear-completion ed) :redraw)
               (t (let* ((lcp (%common-prefix comps))
                         (new-token (if (> (length lcp) (length token)) lcp token)))
                    (when (> (length lcp) (length token)) (replace-token lcp 0))
                    (%set-completion-anchor ed start (+ start (length new-token))
                                            new-token comps nil)
                    (cons :show comps))))))))))

;;; --- Key dispatch (the driver feeds keys here) ----------------------------

(defun %ledit-search-key (ed key)
  "Key dispatch while in ^R reverse-search mode."
  (if (characterp key)
      (progn (ledit-search-type ed key) :redraw)
      (case key
        (:reverse-search (ledit-reverse-search ed) :redraw)   ; older match
        (:backspace      (ledit-search-backspace ed) :redraw)
        (:cancel         (ledit-search-cancel ed) :redraw)    ; ^C: restore the line, stay
        (:enter          (ledit-search-accept ed) :submit)    ; accept + run
        (:eof            (ledit-search-cancel ed) :redraw)    ; ^D: leave search
        ;; any movement/edit key accepts the match, then applies normally
        (t (ledit-search-accept ed) (%ledit-normal-key ed key)))))

(defun %ledit-normal-key (ed key)
  "Key dispatch during ordinary editing (not in search mode)."
  (if (characterp key)
      (progn (ledit-insert ed key) :redraw)
      ;; CASE (not ECASE): an unknown key — including the :redraw that %read-key
      ;; returns for unrecognized escape sequences — is a harmless repaint, never
      ;; a CASE-FAILURE that would crash the editor.
      (case key
        (:backspace   (ledit-backspace ed) :redraw)
        (:delete      (ledit-delete ed) :redraw)
        (:left        (ledit-left ed) :redraw)
        (:right       (unless (ledit-accept-suggestion ed) (ledit-right ed)) :redraw)
        (:home        (ledit-home ed) :redraw)
        (:end         (unless (ledit-accept-suggestion ed) (ledit-end ed)) :redraw)
        (:kill-to-end (ledit-kill-to-end ed) :redraw)
        (:kill-line   (ledit-kill-line ed) :redraw)
        (:kill-word-back    (ledit-kill-word-back ed) :redraw)
        (:kill-word-forward (ledit-kill-word-forward ed) :redraw)
        (:back-word    (ledit-backward-word ed) :redraw)
        (:forward-word (ledit-forward-word ed) :redraw)
        (:yank        (ledit-yank ed) :redraw)
        (:transpose   (ledit-transpose ed) :redraw)
        (:clear       :clear)               ; ^L — driver clears the screen, repaints
        (:reverse-search (ledit-reverse-search ed) :redraw)  ; ^R — enter search mode
        (:prev        (ledit-history-prev ed) :redraw)
        (:next        (ledit-history-next ed) :redraw)
        (:tab         (ledit-complete ed))
        (:enter       :submit)
        (:cancel      :cancel)
        (:eof         (if (zerop (length (ledit-text ed))) :eof :redraw))
        (t            :redraw))))

(defun ledit-key (ed key)
  "Apply KEY to ED, returning an action: :redraw, :submit, :cancel, :eof, :clear,
or (:show . completions).  Routes to the ^R search sub-mode when active.  KEY is
a character (insert) or a keyword."
  (if (ledit-searching ed)
      (%ledit-search-key ed key)
      (%ledit-normal-key ed key)))

;;; ---------------------------------------------------------------------------
;;; Terminal driver (raw mode via stty; interactive tty only)
;;; ---------------------------------------------------------------------------

(defun %terminal-fd-of (stream)
  "The underlying terminal fd for STREAM.  *standard-input* is typically a
SYNONYM- or TWO-WAY-STREAM, not directly an fd-stream, so unwrap those first;
fall back to fd 0 (stdin) when STREAM is an interactive terminal but no fd-stream
is reachable.  NIL if no terminal fd can be found."
  (labels ((unwrap (s)
             (typecase s
               (synonym-stream (unwrap (symbol-value (synonym-stream-symbol s))))
               (two-way-stream (unwrap (two-way-stream-input-stream s)))
               (t s))))
    (or (ignore-errors (sb-sys:fd-stream-fd (unwrap stream)))
        (and (ignore-errors (c-isatty 0)) 0))))

(defun interactive-terminal-p (&optional (stream *standard-input*))
  "True if STREAM is an interactive terminal (so line editing is appropriate).
NIL for a pipe or file — the REPL then uses plain READ-LINE.  Resolves the fd
through SYNONYM-/TWO-WAY-STREAM wrappers (which *standard-input* usually is) and
confirms it with isatty(3) — the naive (sb-sys:fd-stream-fd stream) errors on
those wrappers, which used to wrongly disable the editor in the dumped image."
  (and (interactive-stream-p stream)
       (let ((fd (%terminal-fd-of stream)))
         (and fd (ignore-errors (c-isatty fd))))))

(defun %stty (args)
  "Run stty ARGS on the controlling terminal, waiting for it."
  (ignore-errors (wait-process (launch "stty" args) :timeout 2)))

(defparameter *esc-follower-ms* 60
  "How long to wait for a byte following ESC before deciding it was a lone ESC
key (and for each subsequent byte of an escape sequence).  Escape sequences
arrive as a burst, so this need only cover terminal/pty latency.")

(defun %tty-read (in fd &optional (timeout-ms nil))
  "Read one character from raw-mode terminal IN, resilient to signal-interrupted
reads.  With FD (a real terminal): first LISTEN (a char SBCL already buffered
from an earlier chunked read — poll on the OS fd would miss it); otherwise
poll(2) the fd — indefinitely when TIMEOUT-MS is NIL, else for that many ms — so
a SIGCHLD interrupting the blocking read cannot masquerade as EOF, and a
TIMEOUT-MS lets a lone ESC time out instead of blocking.  Without FD (a string
stream, in tests) it just reads.  Returns the char, or NIL on EOF/timeout."
  (if (null fd)
      (read-char in nil nil)
      (if (or (listen in)                          ; already buffered by SBCL
              (c-poll-readable fd (or timeout-ms -1)))   ; or the OS fd has data
          (read-char in nil nil)
          nil)))                                     ; timeout / EOF

(defun %read-csi (in fd)
  "Read a CSI sequence body (everything after `ESC[`), consuming up to AND
INCLUDING its final byte (0x40-0x7E), then map it to a key.  Consuming the whole
sequence is what keeps an unhandled key's tail (params, `~`, paste payload) from
leaking into the buffer as literal characters.  Each byte is read with a short
timeout so a truncated sequence (or a signal) can't wedge or corrupt parsing.
Returns :redraw for anything unrecognized."
  (let ((params (make-string-output-stream)) (final nil))
    (loop for c = (%tty-read in fd *esc-follower-ms*)
          do (cond ((null c) (return))                       ; timeout/EOF: truncated
                   ((<= 64 (char-code c) 126) (setf final c) (return)) ; final byte
                   (t (write-char c params))))               ; parameter/intermediate
    (let ((p (get-output-stream-string params)))
      (case final
        (#\A :prev) (#\B :next) (#\C :right) (#\D :left)
        (#\H :home) (#\F :end)
        (#\~ (cond ((member p '("1" "7") :test #'string=) :home)
                   ((member p '("4" "8") :test #'string=) :end)
                   ((string= p "3") :delete)
                   (t :redraw)))
        (t :redraw)))))

(defun %read-key (in &optional fd)
  "Read one logical key from raw-mode stream IN (FD is its terminal fd, or NIL for
a string stream): a character, or a keyword for control/navigation keys.  Decodes
the common CSI arrow/Home/End/Delete escapes; unhandled control characters and
escape sequences are ignored (map to :redraw).  Reads are resilient to
signal-interrupted syscalls when FD is given."
  (let ((c (%tty-read in fd)))                   ; block (resiliently) for a key
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
      ((char= c (code-char 23)) :kill-word-back) ; ^W
      ((char= c (code-char 25)) :yank)           ; ^Y
      ((char= c (code-char 12)) :clear)          ; ^L
      ((char= c (code-char 18)) :reverse-search) ; ^R
      ((char= c (code-char 20)) :transpose)      ; ^T
      ((char= c #\Tab) :tab)
      ((char= c #\Escape)
       ;; wait briefly for a following byte: `[` => a CSI sequence; a letter =>
       ;; a Meta (Alt) key; absent (a lone ESC key, or a signal) => ignore rather
       ;; than block/corrupt
       (let ((next (%tty-read in fd *esc-follower-ms*)))
         (cond
           ((null next) :redraw)
           ((char= next #\[) (%read-csi in fd))
           ((char= next #\b) :back-word)          ; M-b
           ((char= next #\f) :forward-word)       ; M-f
           ((char= next #\d) :kill-word-forward)  ; M-d
           ((or (char= next #\Rubout) (char= next #\Backspace)) :kill-word-back) ; M-DEL
           (t :redraw))))
      ;; any other control char (^B ^F ^R ...) is not an insertable character —
      ;; ignore it rather than stuffing a control byte into the line
      ((< (char-code c) 32) :redraw)
      (t c))))

(defun %display-width (string)
  "The visible column width of STRING, skipping ANSI CSI escape sequences (ESC `[`
… final byte 0x40-0x7E) so a colored prompt still positions the cursor correctly
— a raw (length ...) would count the invisible escape bytes and land too far."
  (let ((n (length string)) (i 0) (w 0))
    (loop while (< i n) do
      (let ((c (char string i)))
        (cond
          ((and (char= c #\Escape) (< (1+ i) n) (char= (char string (1+ i)) #\[))
           (incf i 2)                          ; skip ESC [
           (loop while (and (< i n) (not (<= 64 (char-code (char string i)) 126)))
                 do (incf i))                  ; skip parameter/intermediate bytes
           (when (< i n) (incf i)))            ; skip the final byte
          (t (incf w) (incf i)))))
    w))

(defun %redraw (ed prompt out)
  "Repaint the prompt + line and place the cursor, using ANSI escapes.  Any
autosuggestion is drawn dim after the text; the cursor is then placed back before
it so typing continues at point."
  (let ((suggestion (%ledit-suggestion ed)))
    (format out "~C[2K~C~A~A" #\Escape #\Return prompt (ledit-text ed)) ; clear line, home, prompt+text
    (when suggestion
      (format out "~C[90m~A~C[0m" #\Escape suggestion #\Escape))        ; dim grey ghost text
    ;; column = visible prompt width + point (1-based); %display-width so ANSI
    ;; color escapes in the prompt do not skew the cursor position
    (format out "~C[~DG" #\Escape (+ 1 (%display-width prompt) (ledit-point ed)))
    (finish-output out)))

(defun %search-prompt (ed)
  "The transient prompt shown during ^R reverse-search, e.g.
`(reverse-i-search)`git': `; `failed ` prefixes it when the query matches nothing."
  (format nil "(~:[~;failed ~]reverse-i-search)`~A': "
          (ledit-sfailed ed) (ledit-squery ed)))

(defun read-line-edited (prompt &key (in *standard-input*) (out *standard-output*))
  "Read a line with editing/completion/history from raw-mode terminal IN.
Returns the line string, or NIL on EOF."
  (let* ((ed (make-ledit))
         (fd (%terminal-fd-of in))
         ;; snapshot the exact terminal state so we can restore it with a single
         ;; tcsetattr syscall — robust against a preempted `stty sane` subprocess
         (saved (and fd (save-termios fd))))
    (flet ((repaint ()
             ;; while searching, the prompt becomes the incremental-search prompt
             (%redraw ed (if (ledit-searching ed) (%search-prompt ed) prompt) out)))
      (%stty '("-echo" "-icanon" "min" "1" "time" "0"))
      (unwind-protect
           (progn
             (repaint)
             (loop
               (let ((action (ledit-key ed (%read-key in fd))))
                 (case action
                   (:submit (format out "~%") (return (ledit-text ed)))
                   (:cancel (format out "^C~%") (%ledit-set-line ed "")
                            (setf (ledit-hidx ed) nil))
                   (:clear (format out "~C[2J~C[H" #\Escape #\Escape)  ; ^L: wipe + home
                           (repaint))
                   (:eof (format out "~%") (return nil))
                   (t (when (and (consp action) (eq (car action) :show))
                        (format out "~%~{~A~^  ~}~%" (cdr action)))
                      (repaint))))))
        ;; restore the captured attributes atomically; fall back to `stty sane`
        ;; only if we could not snapshot them
        (unless (and fd saved (restore-termios fd saved))
          (%stty '("sane")))))))

;;; ---------------------------------------------------------------------------
;;; The REPL
;;; ---------------------------------------------------------------------------

(defun %read-repl-line (prompt interactive in out)
  (if interactive
      (read-line-edited prompt :in in :out out)
      (progn (write-string prompt out) (finish-output out) (read-line in nil nil))))

(defun shell-repl (&key (in *standard-input*) (out *standard-output*))
  "Read-eval-print loop over surface syntax.  On an interactive tty it uses the
line editor (Tab completion, Up/Down history, Emacs-ish keys); otherwise plain
READ-LINE.  Reports pending job events before each prompt.  Ctrl-C aborts the
line (or, mid-command, tears the job down); Ctrl-D / EOF ends the loop."
  (let ((interactive (interactive-terminal-p in))
        (*present-color* (interactive-terminal-p in)))  ; bold table headers on a tty
    (loop
      (dolist (event (take-job-events)) (format out "~&[~A]~%" event))
      (let ((line (handler-case (%read-repl-line (prompt) interactive in out)
                    (sb-sys:interactive-interrupt () (format out "~&^C~%") "")
                    ;; a non-interrupt error while reading (a stream error, a
                    ;; decode error) is reported and the loop continues, rather
                    ;; than killing the whole REPL
                    (error (e) (format out "~&Error reading input: ~A~%" e) ""))))
        (when (null line) (return))
        (unless (zerop (length (string-trim '(#\Space #\Tab) line)))
          (record-line line)
          (setf *last-status* 0)               ; a foreground pipeline overrides this
          (handler-case (present (shell-eval line) out)
            (shell-exit (c) (return (shell-exit-code c)))
            (sb-sys:interactive-interrupt () (setf *last-status* 130) (format out "~&^C~%"))
            (error (e) (setf *last-status* 1) (format out "~&Error: ~A~%" e))))))))

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
  ;; Restore command history from disk, and persist new lines going forward.
  (load-history-file)
  (setf *history-persist* t)
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
