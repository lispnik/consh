;;;; wrappers/grep.lisp — grep: request -n, parse matches into grep-match objects.
;;;;
;;;; SPEC §2 "Wrappers may ENRICH": rewrite-invocation adds -n (line numbers) —
;;;; and -H when grepping files — so each match arrives as `file:line:text`
;;;; (or `line:text` on stdin).  parse-output turns those into grep-match objects
;;;; (file / line-number / text), so downstream stages can filter by line number
;;;; or jump to a location instead of re-scraping colons.
;;;;
;;;; If the user asked for a mode whose output is NOT line:text (-c, -l, -o, -q),
;;;; we neither add -n nor enrich — the default string-lines parse applies.
;;;;
;;;; grep's exit codes are also non-obvious: 0 = matches, 1 = NO matches (benign,
;;;; not a failure), >=2 = real error.  parse-error-output encodes that so a
;;;; "grep found nothing" pipeline does not raise (SPEC §2 status translation).

(in-package #:consh)

;;; ---------------------------------------------------------------------------
;;; The enriched object
;;; ---------------------------------------------------------------------------

(defclass grep-match ()
  ((file        :initarg :file        :initform nil :reader grep-match-file
                :documentation "Source file, or NIL for stdin / an unnamed match.")
   (line-number :initarg :line-number :reader grep-match-line-number
                :documentation "1-based line number within the file.")
   (text        :initarg :text        :reader grep-match-text
                :documentation "The matched line's content (no trailing newline)."))
  (:documentation "One `grep -n` hit: where the match is, and what matched."))

(defmethod print-object ((m grep-match) stream)
  (print-unreadable-object (m stream :type t)
    (format stream "~@[~A:~]~D ~S"
            (grep-match-file m) (grep-match-line-number m) (grep-match-text m))))

;;; ---------------------------------------------------------------------------
;;; The invocation
;;; ---------------------------------------------------------------------------

(defclass grep-invocation (command-invocation)
  ((pattern :initform nil :accessor grep-pattern)
   (files   :initform nil :accessor grep-files))
  (:documentation "A `grep` call.  Yields grep-match objects when line-numbered."))

(defmethod initialize-instance :after ((c grep-invocation) &key)
  (multiple-value-bind (flags operands) (split-flags (invocation-arguments c))
    (declare (ignore flags))
    (when operands
      (setf (grep-pattern c) (first operands)
            (grep-files c) (rest operands)))))

;;; --- flag inspection (short flags bundle: -rn, -in, ...) -------------------

(defun %grep-short-flag-chars (args)
  "The set of short-flag characters across ARGS — every char of each single-dash
bundle (e.g. \"-rn\" -> r, n).  Long --options and operands are ignored."
  (let ((chars '()))
    (dolist (a args chars)
      (when (and (> (length a) 1)
                 (char= (char a 0) #\-)
                 (char/= (char a 1) #\-))      ; not a long --option
        (loop for ch across a for i from 0 when (plusp i) do (pushnew ch chars))))))

(defun %grep-has-short-p (args char) (member char (%grep-short-flag-chars args)))

(defparameter +grep-non-line-short-flags+ '(#\c #\l #\L #\o #\q)
  "Short flags whose output is not line:text (count / file lists / only-matching /
quiet), so grep is left as plain string lines.")
(defparameter +grep-non-line-long-flags+
  '("--count" "--files-with-matches" "--files-without-match"
    "--only-matching" "--quiet" "--silent"))

(defun %grep-enrichable-p (command)
  "T unless the user selected an output mode that is not one line:text per match."
  (let ((args (invocation-arguments command)))
    (not (or (intersection +grep-non-line-short-flags+ (%grep-short-flag-chars args))
             (some (lambda (f) (flag-present-p args f)) +grep-non-line-long-flags+)))))

(defun %grep-line-numbered-p (command)
  (let ((args (invocation-arguments command)))
    (or (%grep-has-short-p args #\n) (flag-present-p args "--line-number"))))

(defun %grep-filename-fixed-p (args)
  "T if the user already forced (-H) or suppressed (-h) the filename column."
  (or (%grep-has-short-p args #\H) (flag-present-p args "--with-filename")
      (%grep-has-short-p args #\h) (flag-present-p args "--no-filename")))

(defmethod rewrite-invocation ((c grep-invocation))
  "Request -n (and -H when grepping named files) unless the user chose a
non-line output mode.  Flags are prepended so option-ordering is unambiguous."
  (if (not (%grep-enrichable-p c))
      c
      (let ((prefix '()))
        (unless (%grep-line-numbered-p c) (push "-n" prefix))
        (when (and (grep-files c) (not (%grep-filename-fixed-p (invocation-arguments c))))
          (push "-H" prefix))
        (if prefix
            (make-instance 'grep-invocation :program (invocation-program c)
                           :arguments (append prefix (invocation-arguments c)))
            c))))

;;; ---------------------------------------------------------------------------
;;; parse-output: line:text / file:line:text -> grep-match
;;; ---------------------------------------------------------------------------

(defun %all-digits-p (s)
  (and (plusp (length s)) (every #'digit-char-p s)))

(defun %parse-grep-line (command line)
  "Parse one `grep -n` record.  Format-driven: a leading all-digit field means
`line:text` (no filename); otherwise `file:line:text`.  Text may contain colons
— we split only at the boundaries we need."
  (let ((c1 (position #\: line)))
    (unless c1 (signal-parse-error command line))
    (let ((head (subseq line 0 c1)))
      (if (%all-digits-p head)
          (make-instance 'grep-match
                         :file nil
                         :line-number (parse-integer head)
                         :text (subseq line (1+ c1)))
          (let ((c2 (position #\: line :start (1+ c1))))
            (unless c2 (signal-parse-error command line))
            (let ((mid (subseq line (1+ c1) c2)))
              (unless (%all-digits-p mid) (signal-parse-error command line))
              (make-instance 'grep-match
                             :file head
                             :line-number (parse-integer mid)
                             :text (subseq line (1+ c2)))))))))

(defmethod parse-output ((command grep-invocation) stream
                         &key (on-parse-error :error) &allow-other-keys)
  (if (and (%grep-enrichable-p command) (%grep-line-numbered-p command))
      (emitting (emit :on-parse-error on-parse-error)
        (loop for line = (read-line stream nil nil)
              while line
              when (plusp (length line))
                do (funcall emit
                            (parse-record command line
                              (lambda () (%parse-grep-line command line))))))
      (call-next-method)))                 ; plain string lines (default parse)

;;; ---------------------------------------------------------------------------
;;; Exit-code translation (unchanged): 1 = no match, benign.
;;; ---------------------------------------------------------------------------

(defmethod parse-error-output ((command grep-invocation) stream status)
  (cond
    ((and (integerp status) (<= status 1)) nil)   ; 0 = matched, 1 = no match
    (t (make-condition 'command-failed
                       :command command :status status
                       :stderr (slurp-stream stream)))))

(register-wrapper "grep" 'grep-invocation)
