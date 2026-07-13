;;;; wrappers/stat.lisp — stat: dialect-aware flags, uniform parse (SPEC §2).
;;;;
;;;; `stat` is the textbook dialect case: GNU wants `-c '%s %Y %U'`, BSD wants
;;;; `-f '%z %m %Su'`.  So the wrapper PROBES which stat is installed, then
;;;; rewrite-invocation asks it for one fixed "name|size|mtime|owner" format in
;;;; that dialect's spelling — after which parse-output is dialect-agnostic and
;;;; yields the same file-info objects on Linux and macOS.

(in-package #:consh)

(defclass stat-invocation (command-invocation)
  ((files :initform nil :accessor stat-files))
  (:documentation "A `stat` call.  Yields file-info objects, dialect-aware."))

(defmethod initialize-instance :after ((c stat-invocation) &key)
  (multiple-value-bind (flags operands) (split-flags (invocation-arguments c))
    (declare (ignore flags))
    (setf (stat-files c) operands)))

;; PROBE: consult (and cache) which stat this is.
(defmethod command-dialect ((c stat-invocation))
  (ensure-dialect c "stat"))

;; REWRITE: request "name|size|mtime|owner" in the probed dialect's format flag.
(defmethod rewrite-invocation ((c stat-invocation))
  (let ((format (ecase (command-dialect c)
                  (:gnu           (list "-c" "%n|%s|%Y|%U"))
                  ((:bsd :unknown) (list "-f" "%N|%z|%m|%Su")))))
    (make-instance 'stat-invocation :program "stat"
                   :arguments (append format (stat-files c)))))

;; PARSE: uniform after the rewrite.
(defun %parse-stat-line (command line)
  (let* ((p1 (position #\| line))
         (p2 (and p1 (position #\| line :start (1+ p1))))
         (p3 (and p2 (position #\| line :start (1+ p2)))))
    (unless (and p1 p2 p3) (signal-parse-error command line))
    (let ((name  (subseq line 0 p1))
          (size  (parse-integer line :start (1+ p1) :end p2 :junk-allowed t))
          (mtime (parse-integer line :start (1+ p2) :end p3 :junk-allowed t))
          (owner (subseq line (1+ p3))))
      (unless (and size mtime) (signal-parse-error command line))
      (make-instance 'file-info :name name :path (pathname name)
                     :size size :mtime mtime :mode nil :uid nil :gid nil
                     :owner owner))))

(defmethod parse-output ((c stat-invocation) stream
                         &key (on-parse-error :error) &allow-other-keys)
  (emitting (emit :on-parse-error on-parse-error)
    (loop for line = (read-line stream nil nil)
          while line
          when (plusp (length line))
            do (funcall emit (parse-record c line (lambda () (%parse-stat-line c line)))))))

(register-wrapper "stat" 'stat-invocation)
