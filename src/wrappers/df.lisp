;;;; wrappers/df.lisp — df: request portable 1K-block output, parse filesystems.
;;;;
;;;; SPEC §2 format preference: `df -Pk` gives one line per filesystem in fixed
;;;; 1024-byte-block columns on both GNU and BSD, so parse-output can turn each
;;;; into a filesystem object (device / blocks / used / available / capacity /
;;;; mount-point).  Parsing is anchored on the `NN%` capacity column so device
;;;; names or mount points containing spaces (macOS `map auto_home`, `/Volumes/My
;;;; Disk`) still parse.  `df -i` (inode mode) has different columns, so it is
;;;; left as plain string lines.

(in-package #:consh)

;;; ---------------------------------------------------------------------------
;;; The object
;;; ---------------------------------------------------------------------------

(defclass filesystem ()
  ((device      :initarg :device      :reader filesystem-device)
   (blocks      :initarg :blocks      :reader filesystem-blocks
                :documentation "Total size in 1024-byte blocks.")
   (used        :initarg :used        :reader filesystem-used)
   (available   :initarg :available   :reader filesystem-available)
   (capacity    :initarg :capacity    :reader filesystem-capacity
                :documentation "Percent of space used, as an integer.")
   (mount-point :initarg :mount-point :reader filesystem-mount-point))
  (:documentation "One `df -Pk` row: a mounted filesystem and its usage."))

(defmethod print-object ((fs filesystem) stream)
  (print-unreadable-object (fs stream :type t)
    (format stream "~A ~D% (~A)"
            (filesystem-mount-point fs) (filesystem-capacity fs)
            (filesystem-device fs))))

;;; ---------------------------------------------------------------------------
;;; The invocation
;;; ---------------------------------------------------------------------------

(defclass df-invocation (command-invocation) ()
  (:documentation "A `df` call.  Yields filesystem objects."))

(defun %df-inode-mode-p (command)
  (let ((args (invocation-arguments command)))
    (or (short-flag-present-p args #\i) (flag-present-p args "--inodes"))))

(defmethod rewrite-invocation ((command df-invocation))
  "Request -P (portable, one line per fs) and -k (1024-byte blocks) unless the
user chose inode mode (different columns)."
  (if (%df-inode-mode-p command)
      command
      (let ((args (invocation-arguments command))
            (prefix '()))
        (unless (flag-present-p args "-k") (push "-k" prefix))
        (unless (flag-present-p args "-P") (push "-P" prefix))
        (if prefix
            (make-instance 'df-invocation :program (invocation-program command)
                           :arguments (append prefix args))
            command))))

;;; ---------------------------------------------------------------------------
;;; Parsing (capacity-anchored, tolerant of spaces in device / mount point)
;;; ---------------------------------------------------------------------------

(defun %df-percent-field-p (field)
  (let ((n (1- (length field))))
    (and (plusp n) (char= (char field n) #\%)
         (every #'digit-char-p (subseq field 0 n)))))

(defun %df-header-line-p (line)
  (let ((f (split-whitespace line)))
    (and f (string-equal (first f) "Filesystem"))))

(defun %parse-df-line (command line)
  "Parse one `df -Pk` data row.  Find the `NN%` capacity column; the three
integer columns precede it, the device is everything before them, and the mount
point is everything after."
  (let* ((fields (split-whitespace line))
         (cap (position-if #'%df-percent-field-p fields)))
    (unless (and cap (>= cap 3) (< cap (1- (length fields))))
      (signal-parse-error command line))
    (flet ((int (i) (parse-integer (nth i fields) :junk-allowed t)))
      (let ((blocks (int (- cap 3))) (used (int (- cap 2))) (avail (int (- cap 1)))
            (pct (parse-integer (nth cap fields) :junk-allowed t)))
        (unless (and blocks used avail pct) (signal-parse-error command line))
        (make-instance 'filesystem
                       :device (join-with-space (subseq fields 0 (- cap 3)))
                       :blocks blocks :used used :available avail :capacity pct
                       :mount-point (join-with-space (subseq fields (1+ cap))))))))

(defmethod parse-output ((command df-invocation) stream
                         &key (on-parse-error :error) &allow-other-keys)
  (if (%df-inode-mode-p command)
      (call-next-method)                 ; inode columns: leave as string lines
      (emitting (emit :on-parse-error on-parse-error)
        (loop for line = (read-line stream nil nil)
              while line
              for trimmed = (string-trim '(#\Space #\Tab) line)
              when (and (plusp (length trimmed)) (not (%df-header-line-p trimmed)))
                do (funcall emit
                            (parse-record command line
                              (lambda () (%parse-df-line command line))))))))

(register-wrapper "df" 'df-invocation)
