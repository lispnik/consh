;;;; highlight.lisp — as-you-type syntax colouring for the command line.
;;;;
;;;; Split out of surface.lisp.  Loaded after surface (it uses the command-lookup
;;;; and tokenizer helpers there) and before lineedit, whose %redraw calls it.

(in-package #:consh)

;;; ---------------------------------------------------------------------------
;;; Syntax highlighting (as-you-type colouring of the line)
;;; ---------------------------------------------------------------------------
;;;
;;; The escapes carry zero visible width, so the line editor's cursor math (which
;;; counts characters of the raw text) is unaffected.  It must never error on a
;;; partial line, so the whole thing is wrapped and falls back to the raw text.

(defvar *highlight* t
  "When true, the line editor colours the command line as you type: the command
green when it resolves (red when it doesn't), strings yellow, operators and
$VARs cyan.")

(let ((cache nil))
  (defun %path-command-set (&optional refresh)
    "A memoized hash-set of every executable name on $PATH — so highlighting can
tell a valid command from an invalid one without scanning $PATH per keystroke.
Pass REFRESH to rebuild it (e.g. after changing $PATH)."
    (when (or refresh (null cache))
      (let ((set (make-hash-table :test 'equal))
            (path (sb-ext:posix-getenv "PATH")))
        (when path
          (dolist (dir (%split-string path #\:))
            (when (plusp (length dir))
              (dolist (p (ignore-errors
                          (directory (merge-pathnames (make-pathname :name :wild :type :wild)
                                                      (%as-directory dir)))))
                (setf (gethash (file-namestring p) set) t)))))
        (setf cache set)))
    cache))

(defun %command-known-p (name)
  "True when NAME resolves to something runnable: a builtin, alias, wrapper, an
existing path (if it contains /), or an executable on $PATH."
  (or (builtin-p name)
      (nth-value 1 (gethash name *aliases*))
      (nth-value 1 (gethash name *wrappers*))
      (if (find #\/ name)
          (and (%safe-probe (merge-pathnames name *current-directory*)) t)
          (and (gethash name (%path-command-set)) t))))

(defun %sgr (code string)
  (format nil "~C[~Dm~A~C[0m" #\Escape code string #\Escape))

(defun %highlight (text)
  "Return TEXT with ANSI colour escapes for the command word, strings, operators,
and $VARs — adding no visible width.  Never errors: falls back to raw TEXT."
  (if (not *highlight*)
      text
      (handler-case
          (with-output-to-string (out)
            (let ((n (length text)) (i 0))
              ;; leading whitespace, then the command word coloured by validity
              (loop while (and (< i n) (member (char text i) '(#\Space #\Tab)))
                    do (write-char (char text i) out) (incf i))
              (let ((start i))
                (loop while (and (< i n) (not (member (char text i) +word-delimiters+)))
                      do (incf i))
                (when (> i start)
                  (let ((word (subseq text start i)))
                    (write-string (%sgr (if (%command-known-p word) 32 31) word) out))))
              ;; the remainder: strings / operators / variables
              (loop while (< i n) do
                (let ((c (char text i)))
                  (cond
                    ((member c '(#\" #\'))                       ; quoted string -> yellow
                     (let ((j (1+ i)))
                       (loop while (and (< j n) (char/= (char text j) c)) do (incf j))
                       (let ((end (min n (1+ j))))
                         (write-string (%sgr 33 (subseq text i end)) out)
                         (setf i end))))
                    ((member c '(#\| #\< #\> #\&))               ; operator -> cyan
                     (write-string (%sgr 36 (string c)) out) (incf i))
                    ((char= c #\$)                               ; $VAR -> cyan
                     (let ((j (1+ i)))
                       (loop while (and (< j n)
                                        (let ((d (char text j)))
                                          (or (alphanumericp d) (member d '(#\_ #\{ #\})))))
                             do (incf j))
                       (write-string (%sgr 36 (subseq text i j)) out) (setf i j)))
                    (t (write-char c out) (incf i)))))))
        (error () text))))
