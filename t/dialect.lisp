;;;; t/dialect.lisp — command-dialect probing, try-dialect recovery, stat wrapper.

(in-package #:consh/test)

(def-suite dialect :in consh :description "Dialect probing and the stat wrapper.")
(in-suite dialect)

;;; A synthetic wrapper whose PARSING depends on the dialect: GNU output is
;;; "|"-delimited, BSD is space-delimited.  Used to exercise the try-dialect
;;; restart deterministically, without depending on a real command's dialect.
(defclass dtest-invocation (command-invocation) ())

(defun %dtest-sep (dialect) (ecase dialect (:gnu #\|) (:bsd #\Space) (:unknown #\|)))

(defmethod parse-output ((c dtest-invocation) stream
                         &key (on-parse-error :error) &allow-other-keys)
  (emitting (emit :on-parse-error on-parse-error)
    (loop for line = (read-line stream nil nil)
          while line
          do (funcall emit
                      (parse-record c line
                        (lambda ()
                          (let ((pos (position (%dtest-sep (command-dialect c)) line)))
                            (if pos (cons (subseq line 0 pos) (subseq line (1+ pos)))
                                (signal-parse-error c line)))))))))

;;; ===========================================================================
;;; Classification (pure) and probing (spawns `cmd --version`)
;;; ===========================================================================

(test classify-dialect-text
  (is (eq :gnu (consh::%classify-dialect-text "stat (GNU coreutils) 9.4" 0)))
  (is (eq :gnu (consh::%classify-dialect-text "GNU bash, version 5.3" 0)))
  (is (eq :bsd (consh::%classify-dialect-text "stat: illegal option -- -" 0)))
  (is (eq :bsd (consh::%classify-dialect-text "usage: sed [-Ealn] ..." 1)))
  (is (eq :bsd (consh::%classify-dialect-text "" 2)))          ; nonzero exit, no GNU marker
  (is (eq :unknown (consh::%classify-dialect-text "someprog 1.0" 0))))

(test probe-classifies-real-commands
  (reset-dialect-cache)
  ;; a coreutils/BSD tool resolves to one or the other (which depends on the OS)
  (is (member (probe-dialect "stat") '(:gnu :bsd)))
  ;; bash is GNU on every platform (dialect is per-command, not per-OS)
  (is (eq :gnu (probe-dialect "bash")))
  ;; a missing program probes as :unknown, no error
  (is (eq :unknown (probe-dialect "consh-no-such-program-zzzq"))))

(test probe-is-cached
  (reset-dialect-cache)
  (let ((d (probe-dialect "bash")))
    (is (eq d (gethash "bash" consh::*dialect-cache*)))        ; memoized
    (is (eq d (probe-dialect "bash")))))                       ; same on re-ask

(test next-dialect-cycles
  (is (eq :bsd (next-dialect :gnu)))
  (is (eq :gnu (next-dialect :bsd)))
  (is (eq :gnu (next-dialect :unknown))))

(test ensure-dialect-probes-then-caches-on-slot
  (reset-dialect-cache)
  (let ((inv (make-invocation "bash")))
    (is (eq :unknown (invocation-dialect inv)))
    (is (eq :gnu (ensure-dialect inv "bash")))                 ; probes
    (is (eq :gnu (invocation-dialect inv)))))                  ; cached on the slot

;;; ===========================================================================
;;; try-dialect restart: recover when the dialect was wrong
;;; ===========================================================================

(test try-dialect-rotates-and-recovers
  "Parsing space-delimited (BSD) output under a :gnu assumption fails; the
try-dialect restart rotates to :bsd and the re-parse succeeds."
  (let ((inv (make-instance 'dtest-invocation :program "x")))
    (setf (invocation-dialect inv) :gnu)
    (let ((objs (seq-collect
                 (parse-output inv (string-stream-of "a b")
                               :on-parse-error (lambda (c) (declare (ignore c))
                                                 (invoke-restart 'try-dialect))))))
      (is (equal (list (cons "a" "b")) objs))
      (is (eq :bsd (invocation-dialect inv))))))                ; rotated :gnu -> :bsd

(test try-dialect-accepts-explicit-dialect
  (let ((inv (make-instance 'dtest-invocation :program "x")))
    (setf (invocation-dialect inv) :gnu)
    (let ((objs (seq-collect
                 (parse-output inv (string-stream-of "k v")
                               :on-parse-error (lambda (c) (declare (ignore c))
                                                 (invoke-restart 'try-dialect :bsd))))))
      (is (equal (list (cons "k" "v")) objs)))))

;;; ===========================================================================
;;; The stat wrapper: probe -> dialect-specific flags -> uniform parse
;;; ===========================================================================

(test stat-rewrites-flags-per-dialect
  (let ((inv (make-invocation "stat" "/tmp/x")))
    (setf (invocation-dialect inv) :gnu)
    (is (equal '("-c" "%n|%s|%Y|%U" "/tmp/x") (invocation-arguments (rewrite-invocation inv))))
    (setf (invocation-dialect inv) :bsd)
    (is (equal '("-f" "%N|%z|%m|%Su" "/tmp/x") (invocation-arguments (rewrite-invocation inv))))))

(test stat-parses-uniform-format
  (let* ((inv (make-invocation "stat"))
         (objs (seq-collect
                (parse-output inv (make-string-input-stream
                                   (format nil "myfile|10|1700000000|alice~%")))))
         (f (first objs)))
    (is (typep f 'file-info))
    (is (string= "myfile" (file-name f)))
    (is (= 10 (file-size f)))
    (is (= 1700000000 (file-mtime f)))
    (is (string= "alice" (file-owner f)))))

(test stat-malformed-line-signals
  ;; a line without the delimited fields signals a parse-error (which
  ;; use-raw-lines then recovers into a bare string through the producer)
  (let ((inv (make-invocation "stat")))
    (signals parse-error (consh::%parse-stat-line inv "no-pipes-here"))
    (is (equal '("no-pipes-here")
               (seq-collect (parse-output inv (make-string-input-stream
                                               (format nil "no-pipes-here~%"))
                                          :on-parse-error :use-raw-lines))))))

(test stat-end-to-end-auto-dialect
  "Run the real `stat`: the wrapper probes the dialect, rewrites to that dialect's
format flag, and yields a file-info — same result on GNU and BSD."
  (let ((dir (make-temp-dir)))
    (with-open-file (s (merge-pathnames "sized.dat" dir) :direction :output)
      (write-string "0123456789" s))                           ; 10 bytes
    (let* ((path (namestring (merge-pathnames "sized.dat" dir)))
           (objs (pipeline-collect (make-pipeline (list (external (make-invocation "stat" path))))))
           (f (first objs)))
      (is (typep f 'file-info))
      (is (= 10 (file-size f)))
      (is (integerp (file-mtime f)))
      (is (plusp (length (file-owner f)))))))
