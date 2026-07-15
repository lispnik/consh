;;;; lineedit.lisp — an interactive line editor with Tab completion + history.
;;;;
;;;; The editor MODEL (a LEDIT struct and the operations on it) is pure and unit
;;;; tested: keys mutate the buffer/point/history and return an action.  The
;;;; terminal DRIVER (raw mode via stty, reading keystrokes and redrawing with
;;;; ANSI escapes) is a thin layer used only on an interactive tty; the REPL
;;;; falls back to READ-LINE for pipes and scripts.

(in-package #:consh)

;;; SB-UNICODE (east-asian-width + general-category, used by %char-width) is part
;;; of the SBCL core, so it needs no require.

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
  (comp-token "" :type string)           ; token text left by the last completion
  ;; undo history (a stack of (text . point) snapshots) + insert-run coalescing
  (undo-stack nil)                       ; most-recent snapshot first
  (coalescing nil)                       ; T mid insert-run, so it is one undo unit
  ;; yank-pop (M-y) state
  (yank-mark nil)                        ; start index of the last yank, else NIL
  (yank-idx 0 :type fixnum)              ; kill-ring index of the last yank
  ;; multi-row wrapping bookkeeping (linenoise-style refresh)
  (render-oldpos 0 :type fixnum)         ; cursor column-from-prompt-start last paint
  (render-oldrows 0 :type fixnum)        ; physical rows the last paint occupied
  (render-crow 0 :type fixnum))          ; 0-based row the cursor is on within them

(defun make-ledit (&optional (history *line-history*))
  (%make-ledit :history (coerce history 'vector)))

;;; --- undo + edit bookkeeping ----------------------------------------------

(defparameter *undo-max* 200 "Cap on retained undo snapshots per line.")

(defun %save-undo (ed)
  "Push the current (text . point) onto the undo stack, capped."
  (push (cons (ledit-text ed) (ledit-point ed)) (ledit-undo-stack ed))
  (when (> (length (ledit-undo-stack ed)) *undo-max*)
    (setf (ledit-undo-stack ed) (subseq (ledit-undo-stack ed) 0 *undo-max*))))

(defun %note-insert (ed)
  "Before a self-insert: snapshot once per run of inserts (one undo unit)."
  (setf (ledit-yank-mark ed) nil)
  (unless (ledit-coalescing ed)
    (%save-undo ed)
    (setf (ledit-coalescing ed) t)))

(defun %note-edit (ed)
  "Before a discrete edit (kill/yank/transpose/delete): snapshot, end any run."
  (%save-undo ed)
  (setf (ledit-coalescing ed) nil (ledit-yank-mark ed) nil))

(defun %break-run (ed)
  "A non-editing key (movement/history): end the insert run and yank sequence
without pushing an undo snapshot."
  (setf (ledit-coalescing ed) nil (ledit-yank-mark ed) nil))

(defun ledit-undo (ed)
  "^_ : pop the last snapshot, restoring the line and point."
  (let ((prev (pop (ledit-undo-stack ed))))
    (when prev
      (setf (ledit-text ed) (car prev)
            (ledit-point ed) (min (cdr prev) (length (car prev)))
            (ledit-coalescing ed) nil (ledit-yank-mark ed) nil))))

(defun %ledit-eof (ed)
  "^D: delete the character under the cursor mid-line, or signal EOF on an empty
line (readline's delete-char-or-list-or-eof)."
  (cond ((zerop (length (ledit-text ed))) :eof)
        ((< (ledit-point ed) (length (ledit-text ed)))
         (%note-edit ed) (ledit-delete ed) :redraw)
        (t :redraw)))

(defun ledit-insert (ed ch)
  (let ((p (ledit-point ed)))
    (setf (ledit-text ed) (concatenate 'string (subseq (ledit-text ed) 0 p)
                                       (string ch) (subseq (ledit-text ed) p))
          (ledit-point ed) (1+ p))))

(defun ledit-insert-string (ed s)
  "Insert string S at point (used for bracketed paste)."
  (let ((p (ledit-point ed)))
    (setf (ledit-text ed) (concatenate 'string (subseq (ledit-text ed) 0 p)
                                       s (subseq (ledit-text ed) p))
          (ledit-point ed) (+ p (length s)))))

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
  "^Y: insert the most recent kill at point, remembering the region so a
following M-y (yank-pop) can cycle it."
  (let ((s (first *kill-ring*)))
    (when s
      (let ((p (ledit-point ed)))
        (setf (ledit-text ed) (concatenate 'string (subseq (ledit-text ed) 0 p)
                                           s (subseq (ledit-text ed) p))
              (ledit-point ed) (+ p (length s))
              (ledit-yank-mark ed) p            ; start of the yanked region
              (ledit-yank-idx ed) 0)))))

(defun ledit-yank-pop (ed)
  "M-y: only meaningful right after a yank — replace the just-yanked text with
the next entry in the kill ring, cycling."
  (when (and (ledit-yank-mark ed) (> (length *kill-ring*) 1))
    (%save-undo ed)
    (let* ((start (ledit-yank-mark ed))
           (end   (ledit-point ed))
           (next  (mod (1+ (ledit-yank-idx ed)) (length *kill-ring*)))
           (s     (nth next *kill-ring*))
           (text  (ledit-text ed)))
      (setf (ledit-text ed) (concatenate 'string (subseq text 0 start) s (subseq text end))
            (ledit-point ed) (+ start (length s))
            (ledit-yank-idx ed) next))))

(defun %last-arg-of (line)
  "The last whitespace-delimited word of LINE."
  (let* ((trimmed (string-right-trim '(#\Space #\Tab) line))
         (sp (position-if (lambda (c) (member c '(#\Space #\Tab))) trimmed :from-end t)))
    (if sp (subseq trimmed (1+ sp)) trimmed)))

(defun ledit-yank-last-arg (ed)
  "M-.: insert the last argument of the most recent history entry at point."
  (let ((h (ledit-history ed)))
    (when (plusp (length h))
      (let ((arg (%last-arg-of (aref h (1- (length h))))))
        (when (plusp (length arg))
          (let ((p (ledit-point ed)))
            (setf (ledit-text ed) (concatenate 'string (subseq (ledit-text ed) 0 p)
                                               arg (subseq (ledit-text ed) p))
                  (ledit-point ed) (+ p (length arg)))))))))

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

(defun %paste-key-p (key) (and (consp key) (eq (car key) :paste)))

(defun %ledit-search-key (ed key)
  "Key dispatch while in ^R reverse-search mode."
  (cond
    ((characterp key) (ledit-search-type ed key) :redraw)
    ((%paste-key-p key) (ledit-search-accept ed) (%ledit-normal-key ed key))
    (t
     (case key
        (:reverse-search (ledit-reverse-search ed) :redraw)   ; older match
        (:backspace      (ledit-search-backspace ed) :redraw)
        (:cancel         (ledit-search-cancel ed) :redraw)    ; ^C: restore the line, stay
        (:enter          (ledit-search-accept ed) :submit)    ; accept + run
        (:eof            (ledit-search-cancel ed) :redraw)    ; ^D: leave search
        ;; any movement/edit key accepts the match, then applies normally
        (t (ledit-search-accept ed) (%ledit-normal-key ed key))))))

(defun %ledit-normal-key (ed key)
  "Key dispatch during ordinary editing (not in search mode)."
  (cond
    ((characterp key) (%note-insert ed) (ledit-insert ed key) :redraw)
    ;; a bracketed paste: insert the whole payload as one edit (newlines already
    ;; flattened to spaces), so pasted text can't run a command per line
    ((%paste-key-p key) (%note-edit ed) (ledit-insert-string ed (cdr key)) :redraw)
    (t
     ;; CASE (not ECASE): an unknown key — including the :redraw that %read-key
     ;; returns for unrecognized escape sequences — is a harmless repaint, never
     ;; a CASE-FAILURE that would crash the editor.
     (case key
        (:backspace   (%note-edit ed) (ledit-backspace ed) :redraw)
        (:delete      (%note-edit ed) (ledit-delete ed) :redraw)
        (:left        (%break-run ed) (ledit-left ed) :redraw)
        (:right       (%break-run ed)
                      (if (%ledit-suggestion ed)
                          (progn (%note-edit ed) (ledit-accept-suggestion ed))
                          (ledit-right ed))
                      :redraw)
        (:home        (%break-run ed) (ledit-home ed) :redraw)
        (:end         (%break-run ed)
                      (if (%ledit-suggestion ed)
                          (progn (%note-edit ed) (ledit-accept-suggestion ed))
                          (ledit-end ed))
                      :redraw)
        (:kill-to-end (%note-edit ed) (ledit-kill-to-end ed) :redraw)
        (:kill-line   (%note-edit ed) (ledit-kill-line ed) :redraw)
        (:kill-word-back    (%note-edit ed) (ledit-kill-word-back ed) :redraw)
        (:kill-word-forward (%note-edit ed) (ledit-kill-word-forward ed) :redraw)
        (:back-word    (%break-run ed) (ledit-backward-word ed) :redraw)
        (:forward-word (%break-run ed) (ledit-forward-word ed) :redraw)
        (:yank        (%note-edit ed) (ledit-yank ed) :redraw)   ; note-edit clears mark; yank re-sets it
        (:yank-pop    (ledit-yank-pop ed) :redraw)               ; keeps the yank sequence alive
        (:yank-last-arg (%note-edit ed) (ledit-yank-last-arg ed) :redraw)
        (:transpose   (%note-edit ed) (ledit-transpose ed) :redraw)
        (:undo        (%break-run ed) (ledit-undo ed) :redraw)
        (:clear       :clear)               ; ^L — driver clears the screen, repaints
        (:reverse-search (%break-run ed) (ledit-reverse-search ed) :redraw)  ; ^R
        (:prev        (%break-run ed) (ledit-history-prev ed) :redraw)
        (:next        (%break-run ed) (ledit-history-next ed) :redraw)
        (:tab         (ledit-complete ed))
        (:enter       :submit)
        (:cancel      :cancel)
        (:eof         (%ledit-eof ed))      ; delete char mid-line, or EOF on empty
        (t            :redraw)))))

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
                   ((string= p "200") :paste-start)   ; bracketed paste begin
                   ((string= p "201") :paste-end)     ; bracketed paste end
                   (t :redraw)))
        (t :redraw)))))

(defparameter *paste-read-ms* 200
  "How long to wait for the next byte of a bracketed-paste payload before giving
up — pastes arrive as a burst, so this only needs to cover terminal latency.")

(defun %read-paste (in fd)
  "Read a bracketed-paste payload up to its ESC[201~ terminator and return
(:PASTE . TEXT).  Newlines are flattened to spaces so the paste stays one line
and, crucially, does not submit a command per line."
  (let ((out (make-string-output-stream)))
    (loop for c = (%tty-read in fd *paste-read-ms*) do
      (cond
        ((null c) (return))                              ; timeout/EOF: stop
        ((char= c #\Escape)                              ; maybe the ESC[201~ end
         (when (and (eql (%tty-read in fd *esc-follower-ms*) #\[)
                    (eq (%read-csi in fd) :paste-end))
           (return)))
        ((or (char= c #\Newline) (char= c #\Return)) (write-char #\Space out))
        (t (write-char c out))))
    (cons :paste (get-output-stream-string out))))

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
      ((char= c (code-char 2)) :left)            ; ^B
      ((char= c (code-char 6)) :right)           ; ^F
      ((char= c (code-char 16)) :prev)           ; ^P
      ((char= c (code-char 14)) :next)           ; ^N
      ((char= c (code-char 11)) :kill-to-end)    ; ^K
      ((char= c (code-char 21)) :kill-line)      ; ^U
      ((char= c (code-char 23)) :kill-word-back) ; ^W
      ((char= c (code-char 25)) :yank)           ; ^Y
      ((char= c (code-char 31)) :undo)           ; ^_
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
           ((char= next #\[)
            (let ((k (%read-csi in fd)))
              (if (eq k :paste-start) (%read-paste in fd) k)))  ; slurp a bracketed paste
           ((char= next #\b) :back-word)          ; M-b
           ((char= next #\f) :forward-word)       ; M-f
           ((char= next #\d) :kill-word-forward)  ; M-d
           ((char= next #\y) :yank-pop)           ; M-y
           ((or (char= next #\.) (char= next #\_)) :yank-last-arg) ; M-. / M-_
           ((or (char= next #\Rubout) (char= next #\Backspace)) :kill-word-back) ; M-DEL
           (t :redraw))))
      ;; any other control char (^B ^F ^R ...) is not an insertable character —
      ;; ignore it rather than stuffing a control byte into the line
      ((< (char-code c) 32) :redraw)
      (t c))))

(defun %char-width (char)
  "Display columns occupied by CHAR: 0 for combining/format marks and control
characters, 2 for East-Asian wide/fullwidth (most CJK and many emoji), else 1.
This is the wcwidth(3) analogue that keeps the cursor aligned on wide text."
  (let ((code (char-code char)))
    (cond
      ((or (< code 32) (<= #x7f code #x9f)) 0)                         ; C0/C1 controls
      ((member (sb-unicode:general-category char) '(:mn :me :cf)) 0)   ; combining/format
      ((member (sb-unicode:east-asian-width char) '(:w :f)) 2)         ; wide/fullwidth
      (t 1))))

(defun %display-width (string &optional (start 0) (end (length string)))
  "The visible column WIDTH of STRING[START:END]: skips ANSI CSI escape sequences
(ESC `[` … final byte 0x40-0x7E) so a coloured prompt doesn't skew the count, and
sums per-character display columns (wide = 2, combining = 0) so the cursor stays
aligned on CJK/emoji/combining text — a raw (length ...) would land wrong on both."
  (let ((i start) (w 0))
    (loop while (< i end) do
      (let ((c (char string i)))
        (cond
          ((and (char= c #\Escape) (< (1+ i) end) (char= (char string (1+ i)) #\[))
           (incf i 2)                          ; skip ESC [
           (loop while (and (< i end) (not (<= 64 (char-code (char string i)) 126)))
                 do (incf i))                  ; skip parameter/intermediate bytes
           (when (< i end) (incf i)))          ; skip the final byte
          (t (incf w (%char-width c)) (incf i)))))
    w))

(defun %query-terminal-columns ()
  "The terminal width in columns via `stty size` (which prints \"ROWS COLS\" for
the terminal on its standard input), or NIL if it can't be determined."
  (ignore-errors
    (let* ((text (with-output-to-string (s)
                   (sb-ext:run-program "stty" '("size") :input t :output s
                                       :search t :error nil)))
           (parts (%split-string (string-trim '(#\Space #\Tab #\Newline #\Return) text)
                                 #\Space)))
      (when (= 2 (length parts))
        (let ((c (parse-integer (second parts) :junk-allowed t)))
          (and c (plusp c) c))))))

(defun %redraw (ed prompt out cols)
  "Repaint the prompt + line + dim autosuggestion, wrapping at COLS columns, and
place the cursor at point.  A port of linenoise's multi-line refresh, adapted to
count display columns (so wide chars wrap correctly): it steps down to the last
previously-drawn row, clears the old rows bottom-up, reprints, then moves the
cursor to its (row, column).  Row/column state is remembered on ED."
  (let* ((cols  (max 1 cols))
         (raw   (ledit-text ed))
         (point (ledit-point ed))
         (text  (%highlight raw))
         (sug   (or (%ledit-suggestion ed) ""))
         (pcols (%display-width prompt))
         (tcols (%display-width raw))
         (scols (%display-width sug))
         (len   (+ tcols scols))                        ; content columns after prompt
         (pos   (%display-width raw 0 point))           ; cursor columns from prompt start
         ;; edge case: cursor at end AND content exactly fills the last row — the
         ;; terminal leaves the cursor pending-wrap, so force an extra row.
         (edge  (and (= point (length raw)) (zerop scols)
                     (plusp (+ pcols len)) (zerop (mod (+ pcols len) cols))))
         (rows  (+ (max 1 (ceiling (+ pcols len) cols)) (if edge 1 0)))
         (oldrows (ledit-render-oldrows ed))
         (rpos  (1+ (floor (+ pcols (ledit-render-oldpos ed)) cols)))  ; old cursor row, 1-based
         (s (make-string-output-stream)))
    ;; 1. move down to the last row of the previous render
    (when (> (- oldrows rpos) 0)
      (format s "~C[~DB" #\Escape (- oldrows rpos)))
    ;; 2. clear each old row from the bottom up
    (dotimes (j (max 0 (1- oldrows)))
      (format s "~C~C[0K~C[1A" #\Return #\Escape #\Escape))
    ;; 3. clear the top row
    (format s "~C~C[0K" #\Return #\Escape)
    ;; 4. prompt + highlighted text + dim suggestion
    (write-string prompt s)
    (write-string text s)
    (unless (zerop scols) (format s "~C[90m~A~C[0m" #\Escape sug #\Escape))
    ;; 5. force the wrap when we filled the last column at end of line
    (when edge (format s "~C~C" #\Return #\Newline))
    ;; 6/7. move up to the cursor's row, then to its column
    (let ((rpos2 (1+ (floor (+ pcols pos) cols)))
          (col   (mod (+ pcols pos) cols)))
      (when (> (- rows rpos2) 0) (format s "~C[~DA" #\Escape (- rows rpos2)))
      (write-char #\Return s)
      (when (plusp col) (format s "~C[~DC" #\Escape col))
      (setf (ledit-render-crow ed) (1- rpos2)))
    (setf (ledit-render-oldpos ed) pos
          (ledit-render-oldrows ed) rows)
    (write-string (get-output-stream-string s) out)
    (finish-output out)))

(defun %end-of-content (ed out)
  "Move the cursor to column 0 on the last wrapped row, so following output
(a newline, a completion list) begins below the whole input, not mid-line."
  (let ((below (- (ledit-render-oldrows ed) 1 (ledit-render-crow ed))))
    (when (plusp below) (format out "~C[~DB" #\Escape below)))
  (write-char #\Return out))

;;; --- SIGWINCH via a self-pipe ---------------------------------------------
;;; The read loop poll(2)s the terminal AND a pipe; the SIGWINCH handler writes
;;; one byte to the pipe, so a terminal resize wakes the poll deterministically
;;; (unlike a bare EINTR, which the poll must retry to survive the reaper's
;;; SIGCHLD).  The loop then re-measures the terminal and repaints at the new
;;; width.

(defvar *winch-write-fd* nil
  "Write end of the SIGWINCH self-pipe while a line is being edited, else NIL.")

(defvar *winch-installed* nil "T once the SIGWINCH handler has been installed.")

(defun %winch-handler (&rest args)
  (declare (ignore args))
  (when *winch-write-fd* (ignore-errors (c-write-byte *winch-write-fd*))))

(defun %ensure-winch-handler ()
  "Install the SIGWINCH handler once; it no-ops unless a line edit is active."
  (unless *winch-installed*
    (ignore-errors (sb-sys:enable-interrupt +sigwinch+ #'%winch-handler))
    (setf *winch-installed* t)))

(defun %wait-for-input (in fd winch-fd)
  "Block until a key can be read from FD (or SBCL has one buffered) or a resize
byte arrives on WINCH-FD.  Returns :KEY or :RESIZED.  With no terminal fd (a
string stream, in tests) or no self-pipe, always :KEY (the caller then blocks in
the ordinary read)."
  (cond
    ((or (null fd) (null winch-fd)) :key)
    ((listen in) :key)                          ; SBCL already buffered a key
    (t (loop (case (c-poll-two fd winch-fd -1)
               (:first  (return :key))
               (:second (return :resized))
               (t nil))))))                       ; spurious wake: keep waiting

(defun %hard-repaint (ed prompt out cols)
  "Repaint from scratch after a resize: go to the top of the input area, clear
downward, drop the stale wrap bookkeeping, and redraw at the new width."
  (let ((up (ledit-render-crow ed)))
    (write-char #\Return out)                           ; column 0
    (when (plusp up) (format out "~C[~DA" #\Escape up))  ; up to the first row
    (format out "~C[J" #\Escape))                       ; clear to end of screen
  (setf (ledit-render-oldrows ed) 0
        (ledit-render-oldpos ed) 0
        (ledit-render-crow ed) 0)
  (%redraw ed prompt out cols))

(defun %search-prompt (ed)
  "The transient prompt shown during ^R reverse-search, e.g.
`(reverse-i-search)`git': `; `failed ` prefixes it when the query matches nothing."
  (format nil "(~:[~;failed ~]reverse-i-search)`~A': "
          (ledit-sfailed ed) (ledit-squery ed)))

(defun read-line-edited (prompt &key (in *standard-input*) (out *standard-output*)
                                     abort-on-cancel)
  "Read a line with editing/completion/history from raw-mode terminal IN.
Returns the line string, NIL on EOF, or (when ABORT-ON-CANCEL) :cancelled if the
line was aborted with ^C — used to bail out of a multi-line continuation."
  (let* ((ed (make-ledit))
         (fd (%terminal-fd-of in))
         ;; terminal width for wrapping long lines; re-queried on SIGWINCH
         (cols (or (%query-terminal-columns) 80))
         ;; SIGWINCH self-pipe (only with a real terminal): the handler writes a
         ;; byte to WINCH-WRITE, the read loop watches WINCH-READ alongside the tty
         (winch (and fd (ignore-errors (multiple-value-list (make-pipe)))))
         (winch-read (first winch))
         (winch-write (second winch))
         ;; snapshot the exact terminal state so we can restore it with a single
         ;; tcsetattr syscall — robust against a preempted `stty sane` subprocess
         (saved (and fd (save-termios fd))))
    (when winch
      (set-nonblocking winch-read)          ; so draining an empty pipe won't block
      (set-nonblocking winch-write)          ; so the handler's write can't block
      (%ensure-winch-handler))
    (flet ((repaint ()
             ;; while searching, the prompt becomes the incremental-search prompt
             (%redraw ed (if (ledit-searching ed) (%search-prompt ed) prompt) out cols)))
      (%stty '("-echo" "-icanon" "min" "1" "time" "0"))
      (format out "~C[?2004h" #\Escape)      ; enable bracketed paste
      (finish-output out)
      (let ((*winch-write-fd* winch-write))   ; the handler writes here while we edit
       (unwind-protect
           (progn
             (repaint)
             (loop
               (if (and winch-read
                        (eq :resized (%wait-for-input in fd winch-read)))
                 ;; terminal resized: drain the pipe, re-measure, repaint fresh
                 (progn (c-drain-fd winch-read)
                        (setf cols (or (%query-terminal-columns) cols))
                        (%hard-repaint ed (if (ledit-searching ed) (%search-prompt ed) prompt)
                                       out cols))
               (let ((action (ledit-key ed (%read-key in fd))))
                 (case action
                   (:submit (%end-of-content ed out) (format out "~%")
                            (return (ledit-text ed)))
                   (:cancel (%end-of-content ed out) (format out "^C~%")
                            (if abort-on-cancel
                                (return :cancelled)          ; bail out of continuation
                                (progn (%ledit-set-line ed "")
                                       (setf (ledit-render-oldrows ed) 0
                                             (ledit-render-oldpos ed) 0
                                             (ledit-render-crow ed) 0
                                             (ledit-hidx ed) nil)
                                       (repaint))))
                   (:clear (format out "~C[2J~C[H" #\Escape #\Escape)  ; ^L: wipe + home
                           (setf (ledit-render-oldrows ed) 0
                                 (ledit-render-oldpos ed) 0
                                 (ledit-render-crow ed) 0)
                           (repaint))
                   (:eof (%end-of-content ed out) (format out "~%") (return nil))
                   (t (when (and (consp action) (eq (car action) :show))
                        (%end-of-content ed out)
                        (format out "~%~{~A~^  ~}~%" (cdr action))
                        (setf (ledit-render-oldrows ed) 0
                              (ledit-render-oldpos ed) 0
                              (ledit-render-crow ed) 0))
                      (repaint)))))))
         (format out "~C[?2004l" #\Escape)    ; disable bracketed paste
         (finish-output out)
         ;; restore the captured attributes atomically; fall back to `stty sane`
         ;; only if we could not snapshot them
         (unless (and fd saved (restore-termios fd saved))
           (%stty '("sane")))
         (when winch                          ; close the SIGWINCH self-pipe
           (ignore-errors (c-close winch-read))
           (ignore-errors (c-close winch-write))))))))

;;; ---------------------------------------------------------------------------
;;; The REPL
;;; ---------------------------------------------------------------------------

(defun %read-repl-line (prompt interactive in out &key abort-on-cancel)
  (if interactive
      (read-line-edited prompt :in in :out out :abort-on-cancel abort-on-cancel)
      (progn (write-string prompt out) (finish-output out) (read-line in nil nil))))

(defun %read-complete-input (interactive in out)
  "Read one complete command, spanning continuation lines while the accumulated
input is an incomplete prefix (an open Lisp form or an unterminated quote).
Returns the full text, NIL on EOF, or \"\" if a continuation is aborted with ^C."
  (let ((first (%read-repl-line (prompt) interactive in out)))
    (if (null first)
        nil
        (let ((buffer first))
          (loop until (input-complete-p buffer) do
            (let ((more (%read-repl-line *continuation-prompt* interactive in out
                                         :abort-on-cancel t)))
              (cond ((null more) (return))                    ; EOF: eval what we have
                    ((eq more :cancelled) (return-from %read-complete-input ""))
                    (t (setf buffer (concatenate 'string buffer (string #\Newline) more))))))
          buffer))))

(defun shell-repl (&key (in *standard-input*) (out *standard-output*))
  "Read-eval-print loop over surface syntax.  On an interactive tty it uses the
line editor (Tab completion, Up/Down history, Emacs-ish keys); otherwise plain
READ-LINE.  Reports pending job events before each prompt.  Ctrl-C aborts the
line (or, mid-command, tears the job down); Ctrl-D / EOF ends the loop."
  (let ((interactive (interactive-terminal-p in))
        (*present-color* (interactive-terminal-p in)))  ; bold table headers on a tty
    (loop
      (dolist (event (take-job-events)) (format out "~&[~A]~%" event))
      (let ((line (handler-case (%read-complete-input interactive in out)
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

(defun %run-interactive ()
  "The interactive REPL session: banner, init file, history, job control.
Returns the exit code (so `exit N` at the prompt propagates)."
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
  (prog1
      (unwind-protect
           (handler-case (let ((*package* (find-package '#:consh))) (or (shell-repl) 0))
             (sb-sys:interactive-interrupt () (terpri) 0))
        (disable-terminal-job-control))
    (finish-output)))

(defun %with-script (name args thunk)
  "Run THUNK as a script named NAME with ARGS as positional parameters, in the
CONSH package.  Returns the exit code: a SHELL-EXIT's code, else the final
*last-status* (0 on success)."
  (let ((*script-name* name) (*script-args* args) (*last-status* 0)
        (*package* (find-package '#:consh)))
    (handler-case (progn (funcall thunk) (or *last-status* 0))
      (shell-exit (c) (shell-exit-code c)))))

(defun %string-line-reader (string)
  "A NEXT-LINE thunk yielding successive newline-separated lines of STRING."
  (let ((in (make-string-input-stream string)))
    (lambda () (read-line in nil nil))))

(defun %run-command-string (cmd args)
  "consh -c CMD [name arg...]: run CMD with $0=name (or \"consh\") and $1.. from
the remaining args."
  (%with-script (or (first args) "consh") (rest args)
                (lambda () (%eval-script-lines (%string-line-reader (or cmd ""))))))

(defun %run-script-stream (stream name args)
  "Run a script read from STREAM (e.g. stdin), with NAME as $0 and ARGS as $1.."
  (%with-script name args
                (lambda () (%eval-script-lines (lambda () (read-line stream nil nil))))))

(defun %run-script-file (path args)
  "Run the consh script at PATH with ARGS as positional parameters."
  (let ((file (ignore-errors (probe-file (merge-pathnames path *current-directory*)))))
    (if (null file)
        (progn (format *error-output* "consh: ~A: no such file~%" path) 127)
        (%with-script (namestring file) args
                      (lambda ()
                        (with-open-file (s file)
                          (%eval-script-lines (lambda () (read-line s nil nil)))))))))

(defun %print-usage (&optional (out *standard-output*))
  (format out "~
Usage: consh                     start an interactive shell
       consh SCRIPT [ARG...]      run a consh script with positional args
       consh -c COMMAND [NAME ARG...]  run COMMAND
       consh -                    run a script read from standard input
       consh -h | --help          show this help

In a script, $0 is the script name, $1.. the arguments, $# their count, $@/$*
all of them, and $? the last exit status; (script-args) and (parse-args ...) are
the Lisp equivalents.~%"))

(defun main ()
  "Entry point for a dumped consh executable.  With no arguments it starts the
interactive REPL; otherwise it runs a script file, a -c command string, or a
script from stdin (-), then exits with that script's status."
  ;; A saved image baked in *current-directory* at build time; adopt the real
  ;; working directory the executable was launched from.
  (ignore-errors
   (setf *current-directory* (truename (pathname (format nil "~A/" (sb-posix:getcwd))))))
  (let ((args (rest sb-ext:*posix-argv*)))            ; drop argv[0]
    (sb-ext:exit
     :code
     (handler-case
         (cond
           ((null args) (%run-interactive))
           ((member (first args) '("-h" "--help") :test #'string=) (%print-usage) 0)
           ((string= (first args) "-c") (%run-command-string (second args) (cddr args)))
           ((string= (first args) "-") (%run-script-stream *standard-input* "-" (rest args)))
           (t (%run-script-file (first args) (rest args))))
       (shell-exit (c) (shell-exit-code c))
       (serious-condition (e) (format *error-output* "~&consh: ~A~%" e) 1)))))
