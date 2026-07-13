;;;; wrappers/ps.lisp — ps: request fixed columns, yield process objects.
;;;;
;;;; ps has no JSON/porcelain mode, but its `-o field=,...` selector — with an
;;;; empty header (`=`) to suppress the header row — is portable across BSD and
;;;; procps and lets us pin exactly the columns we parse.  The wrapper rewrites
;;;; `ps` to request `pid ppid user rss stat command` (command last, so it may
;;;; contain spaces) and parses each row into a ps-process object.  A user who
;;;; supplies their own -o keeps control and gets plain lines.

(in-package #:consh)

;;; ---------------------------------------------------------------------------
;;; The process object
;;; ---------------------------------------------------------------------------

(defclass ps-process ()
  ((pid     :initarg :pid     :reader ps-process-pid)
   (ppid    :initarg :ppid    :reader ps-process-ppid)
   (user    :initarg :user    :reader ps-process-user)
   (rss     :initarg :rss     :reader ps-process-rss   :documentation "Resident set size, KB.")
   (state   :initarg :state   :reader ps-process-state :documentation "Symbolic state, e.g. \"S+\".")
   (command :initarg :command :reader ps-process-command))
  (:documentation "One row of `ps`, enriched into an object."))

(defmethod print-object ((p ps-process) stream)
  (print-unreadable-object (p stream :type t)
    (format stream "~A ~A ~A" (ps-process-pid p) (ps-process-user p)
            (let ((c (ps-process-command p)))
              (if (> (length c) 40) (concatenate 'string (subseq c 0 37) "...") c)))))

;;; ---------------------------------------------------------------------------
;;; The invocation
;;; ---------------------------------------------------------------------------

(defparameter +ps-format+ "pid=,ppid=,user=,rss=,stat=,command="
  "The -o column selector the wrapper requests (portable BSD/procps; command
last so it can hold spaces).")

(defclass ps-invocation (command-invocation) ()
  (:documentation "A `ps` call.  Yields ps-process objects."))

(defun %ps-uses-our-format-p (c)
  "True once the invocation carries OUR imposed column format (so we parse it)."
  (member +ps-format+ (invocation-arguments c) :test #'string=))

(defmethod rewrite-invocation ((c ps-invocation))
  ;; leave a user-supplied -o alone; otherwise impose our parseable columns
  (if (flag-present-p (invocation-arguments c) "-o")
      c
      (make-instance 'ps-invocation :program "ps"
                     :arguments (append (invocation-arguments c) (list "-o" +ps-format+)))))

(defun %split-n-fields (line n)
  "Split off the first N whitespace-separated fields of LINE; return (values
field-list rest-of-line-trimmed)."
  (let ((fields '()) (i 0) (len (length line)))
    (flet ((ws-p (c) (member c '(#\Space #\Tab))))
      (dotimes (k n)
        (loop while (and (< i len) (ws-p (char line i))) do (incf i))
        (let ((start i))
          (loop while (and (< i len) (not (ws-p (char line i)))) do (incf i))
          (when (> i start) (push (subseq line start i) fields))))
      (loop while (and (< i len) (ws-p (char line i))) do (incf i)))
    (values (nreverse fields) (subseq line i))))

(defun %parse-ps-line (command line)
  (multiple-value-bind (fields cmd) (%split-n-fields (string-left-trim '(#\Space #\Tab) line) 5)
    (unless (= 5 (length fields)) (signal-parse-error command line))
    (destructuring-bind (pid ppid user rss state) fields
      (flet ((num (s) (parse-integer s :junk-allowed t)))
        (make-instance 'ps-process
                       :pid (num pid) :ppid (num ppid) :user user
                       :rss (num rss) :state state :command cmd)))))

(defmethod parse-output ((c ps-invocation) stream
                         &key (on-parse-error :error) &allow-other-keys)
  ;; Parse our columns only when we imposed them; a user's own -o gets lines.
  (if (%ps-uses-our-format-p c)
      (emitting (emit :on-parse-error on-parse-error)
        (loop for line = (read-line stream nil nil)
              while line
              when (plusp (length (string-trim '(#\Space #\Tab) line)))
                do (funcall emit (parse-record c line (lambda () (%parse-ps-line c line))))))
      (call-next-method)))

(register-wrapper "ps" 'ps-invocation)
