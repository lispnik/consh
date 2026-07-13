;;;; wrappers/du.lisp — du: request 1K blocks, parse "size<TAB>path" into objects.
;;;;
;;;; `du` prints "SIZE\tPATH".  The default block size differs (GNU 1024, BSD
;;;; 512), so rewrite-invocation adds -k for a uniform 1024-byte block on both;
;;;; parse-output then splits on the TAB into du-entry objects (blocks / path).

(in-package #:consh)

(defclass du-entry ()
  ((blocks :initarg :blocks :reader du-entry-blocks
           :documentation "Disk usage in 1024-byte blocks.")
   (path   :initarg :path   :reader du-entry-path))
  (:documentation "One `du -k` row: a path and its disk usage."))

(defmethod print-object ((e du-entry) stream)
  (print-unreadable-object (e stream :type t)
    (format stream "~A ~DK" (du-entry-path e) (du-entry-blocks e))))

(defclass du-invocation (command-invocation) ()
  (:documentation "A `du` call.  Yields du-entry objects."))

(defmethod rewrite-invocation ((command du-invocation))
  "Request -k (1024-byte blocks) for a uniform unit across GNU and BSD."
  (let ((args (invocation-arguments command)))
    (if (or (short-flag-present-p args #\k) (flag-present-p args "--block-size"))
        command
        (make-instance 'du-invocation :program (invocation-program command)
                       :arguments (append '("-k") args)))))

(defun %parse-du-line (command line)
  (let ((tab (position #\Tab line)))
    (unless tab (signal-parse-error command line))
    (let ((blocks (parse-integer line :end tab :junk-allowed t)))
      (unless blocks (signal-parse-error command line))
      (make-instance 'du-entry :blocks blocks :path (subseq line (1+ tab))))))

(defmethod parse-output ((command du-invocation) stream
                         &key (on-parse-error :error) &allow-other-keys)
  (emitting (emit :on-parse-error on-parse-error)
    (loop for line = (read-line stream nil nil)
          while line
          when (plusp (length line))
            do (funcall emit
                        (parse-record command line
                          (lambda () (%parse-du-line command line)))))))

(register-wrapper "du" 'du-invocation)
