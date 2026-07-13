;;;; t/parse.lisp — Phase 3: the per-command parser protocol.

(in-package #:consh/test)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :sb-posix))

(def-suite parse :in consh :description "Parser protocol, wrappers, restarts.")
(in-suite parse)

(defun string-stream-of (&rest lines)
  "An input stream of LINES, each newline-terminated."
  (make-string-input-stream
   (with-output-to-string (s) (dolist (l lines) (write-line l s)))))

;;; A tiny out-of-tree-style wrapper defined entirely here — demonstrating that
;;; a new command needs no change to the shell core (SPEC.md §2): subclass,
;;; add a parse-output method, register.  `consh-kv` parses "key=value" lines
;;; into conses and signals PARSE-ERROR on a line lacking '='.
(defclass kv-invocation (command-invocation) ())

(defmethod parse-output ((command kv-invocation) stream
                         &key (on-parse-error :error) &allow-other-keys)
  (emitting (emit :on-parse-error on-parse-error)
    (loop for line = (read-line stream nil nil)
          while line
          when (plusp (length line))
            do (funcall emit
                        (parse-record command line
                          (lambda ()
                            (let ((pos (position #\= line)))
                              (if pos
                                  (cons (subseq line 0 pos) (subseq line (1+ pos)))
                                  (signal-parse-error command line)))))))))

(register-wrapper "consh-kv" 'kv-invocation)

;;; ---------------------------------------------------------------------------
;;; Lazy channel-backed object sequences
;;; ---------------------------------------------------------------------------

(test unknown-command-yields-string-lines
  "Acceptance: an unregistered command's output is a lazy sequence of strings."
  (let ((inv (make-invocation "wc" "-l")))
    (is (typep inv 'command-invocation))
    (is (not (typep inv 'ls-invocation)))
    (is (equal '("line one" "line two")
               (seq-collect (parse-output inv (string-stream-of "line one" "line two")))))))

(test seq-take-then-collect
  "seq-take yields a prefix and leaves the sequence open for seq-collect."
  (let ((seq (parse-output (make-invocation "wc")
                           (string-stream-of "a" "b" "c" "d"))))
    (is (equal '("a" "b") (seq-take 2 seq)))
    (is (equal '("c" "d") (seq-collect seq)))))

(test seq-next-signals-end
  (let ((seq (parse-output (make-invocation "wc") (string-stream-of "only"))))
    (multiple-value-bind (v more) (seq-next seq)
      (is (string= "only" v))
      (is-true more))
    (multiple-value-bind (v more) (seq-next seq)
      (is (null v))
      (is-false more))))

(test do-object-seq-iterates
  (let ((seq (parse-output (make-invocation "wc") (string-stream-of "1" "2" "3")))
        (acc '()))
    (do-object-seq (x seq) (push x acc))
    (is (equal '("1" "2" "3") (nreverse acc)))))

(test take-kills-producer
  "take grabs a prefix of an INFINITE producer and then kills it — the producer
thread must terminate (downstream cancellation via close-for-reading)."
  (let ((seq (spawn-object-seq
              (lambda (emit) (loop for i from 0 do (funcall emit i)))
              :capacity 4)))
    (is (equal '(0 1 2 3 4) (take 5 seq)))
    (is-false (sb-thread:thread-alive-p (object-seq-thread seq)))))

(test seq-close-is-safe-and-terminates
  (let ((seq (spawn-object-seq
              (lambda (emit) (loop for i from 0 do (funcall emit i)))
              :capacity 2)))
    (is (equal '(0 1) (seq-take 2 seq)))
    (seq-close seq)
    (is-false (sb-thread:thread-alive-p (object-seq-thread seq)))))

;;; ---------------------------------------------------------------------------
;;; Invocation registry and flag parsing
;;; ---------------------------------------------------------------------------

(test registry-dispatches-by-program
  (is (typep (make-invocation "ls")   'ls-invocation))
  (is (typep (make-invocation "find") 'find-invocation))
  (is (typep (make-invocation "cat")  'cat-invocation))
  (is (typep (make-invocation "grep") 'grep-invocation))
  ;; unknown program -> the generic base class, exactly
  (is (eq 'command-invocation (type-of (make-invocation "nosuchcmd")))))

(test invocation-carries-program-and-args
  (let ((inv (make-invocation "wc" "-l" "file")))
    (is (string= "wc" (invocation-program inv)))
    (is (equal '("-l" "file") (invocation-arguments inv)))))

(test split-flags-partitions
  (multiple-value-bind (f o) (split-flags '("-l" "-a" "foo" "bar"))
    (is (equal '("-l" "-a") f))
    (is (equal '("foo" "bar") o)))
  ;; "--" ends flag parsing
  (multiple-value-bind (f o) (split-flags '("-x" "--" "-notaflag"))
    (is (equal '("-x") f))
    (is (equal '("-notaflag") o)))
  ;; a lone "-" is an operand (stdin), not a flag
  (multiple-value-bind (f o) (split-flags '("-"))
    (is (null f))
    (is (equal '("-") o))))

(test ls-parses-flags-and-paths
  (let ((inv (make-invocation "ls" "-l" "/tmp")))
    (is-true (ls-long-p inv))
    (is (equal '("/tmp") (ls-paths inv))))
  (let ((inv (make-invocation "ls")))
    (is-false (ls-long-p inv))
    (is (equal '(".") (ls-paths inv)))))

;;; ---------------------------------------------------------------------------
;;; ls enrichment: bare names -> file objects (size / mtime / owner)
;;; ---------------------------------------------------------------------------

(test ls-enriches-to-file-objects
  "Acceptance: ls yields file objects with size, mtime, and owner."
  (let* ((dir (make-temp-dir)))
    (with-open-file (s (merge-pathnames "a.txt" dir) :direction :output)
      (write-string "hello" s))                     ; 5 bytes
    (with-open-file (s (merge-pathnames "b.txt" dir) :direction :output)
      (write-string "worldwide" s))                 ; 9 bytes
    (let* ((inv (make-instance 'ls-invocation :directory dir))
           (objs (seq-collect (parse-output inv (string-stream-of "a.txt" "b.txt")))))
      (is (= 2 (length objs)))
      (is (every (lambda (o) (typep o 'file-info)) objs))
      (let ((fa (find "a.txt" objs :key #'file-name :test #'string=))
            (fb (find "b.txt" objs :key #'file-name :test #'string=)))
        (is (= 5 (file-size fa)))
        (is (= 9 (file-size fb)))
        (is (integerp (file-mtime fa)))
        (is (plusp (file-mtime fa)))
        (is (stringp (file-owner fa)))
        (is (plusp (length (file-owner fa))))
        ;; owner resolves to the user actually running the tests
        (is (equal (uid-username (sb-posix:getuid)) (file-owner fa)))))))

(test ls-enriches-real-ls-output
  "End-to-end: spawn a real `ls`, pipe its stdout into parse-output, enrich."
  (let ((dir (make-temp-dir)))
    (with-open-file (s (merge-pathnames "one.txt" dir) :direction :output)
      (write-string "12345" s))                     ; 5 bytes
    (with-open-file (s (merge-pathnames "two.txt" dir) :direction :output)
      (write-string "678" s))                       ; 3 bytes
    (multiple-value-bind (r w) (make-pipe)
      (let ((in (sb-sys:make-fd-stream r :input t :element-type 'character))
            (p (launch "ls" (list (namestring dir))
                       :file-actions (list (list :dup2 w 1)))))
        (c-close w)
        (unwind-protect
             (let* ((inv (make-instance 'ls-invocation :directory dir))
                    (objs (seq-collect
                           (parse-output inv in :on-parse-error :use-raw-lines))))
               (wait-process p :timeout 5)
               (is (equal '("one.txt" "two.txt")
                          (sort (mapcar #'file-name objs) #'string<)))
               (let ((one (find "one.txt" objs :key #'file-name :test #'string=)))
                 (is (typep one 'file-info))
                 (is (= 5 (file-size one)))))
          (close in))))))

;;; ---------------------------------------------------------------------------
;;; parse-error + restarts (the acceptance recovery path)
;;; ---------------------------------------------------------------------------

(test ls-use-raw-lines-recovers-from-malformed
  "Acceptance: forced malformed input recovers via the use-raw-lines restart —
a name that cannot be stat'd falls back to the bare string, valid names stay
enriched."
  (let ((dir (make-temp-dir)))
    (with-open-file (s (merge-pathnames "real.txt" dir) :direction :output)
      (write-string "x" s))
    (let* ((inv (make-instance 'ls-invocation :directory dir))
           (objs (seq-collect
                  (parse-output inv (string-stream-of "real.txt" "GHOST" "real.txt")
                                :on-parse-error :use-raw-lines))))
      (is (= 3 (length objs)))
      (is (typep (first objs) 'file-info))
      (is (stringp (second objs)))                  ; the ghost fell back to raw
      (is (string= "GHOST" (second objs)))
      (is (typep (third objs) 'file-info)))))

(test on-parse-error-function-policy
  "An :on-parse-error function sees the condition and may choose a restart."
  (let ((dir (make-temp-dir))
        (seen '()))
    (with-open-file (s (merge-pathnames "real.txt" dir) :direction :output)
      (write-string "x" s))
    (let* ((inv (make-instance 'ls-invocation :directory dir))
           (objs (seq-collect
                  (parse-output inv (string-stream-of "GHOST" "real.txt")
                                :on-parse-error
                                (lambda (c)
                                  (push (parse-error-raw c) seen)
                                  (invoke-restart 'use-raw-lines))))))
      (is (equal '("GHOST") seen))
      (is (= 2 (length objs)))
      (is (string= "GHOST" (first objs)))
      (is (typep (second objs) 'file-info)))))

(test parse-record-success-passes-through
  (is (= 42 (parse-record nil "42" (lambda () (parse-integer "42"))))))

(test parse-record-use-raw-lines
  (is (string= "raw"
               (handler-bind ((parse-error (lambda (c) (declare (ignore c))
                                             (invoke-restart 'use-raw-lines))))
                 (parse-record nil "raw" (lambda () (signal-parse-error nil "raw")))))))

(test parse-record-define-parser
  (is (equal '(:parsed "z")
             (handler-bind ((parse-error
                              (lambda (c) (declare (ignore c))
                                (invoke-restart 'define-parser
                                                (lambda (r) (list :parsed r))))))
               (parse-record nil "z" (lambda () (signal-parse-error nil "z")))))))

(test parse-record-try-dialect-reruns-thunk
  (let ((calls 0))
    (is (= 7 (handler-bind ((parse-error (lambda (c) (declare (ignore c))
                                           (invoke-restart 'try-dialect))))
               (parse-record nil "x"
                             (lambda ()
                               (incf calls)
                               (if (= calls 1) (signal-parse-error nil "x") 7))))))
    (is (= 2 calls))))

;;; ---------------------------------------------------------------------------
;;; parse-error-output / command-failed  (status translation)
;;; ---------------------------------------------------------------------------

(test parse-error-output-default
  (let ((c (parse-error-output (make-invocation "wc") (make-string-input-stream "boom") 2)))
    (is (typep c 'command-failed))
    (is (= 2 (command-failed-status c)))
    (is (string= "boom" (command-failed-stderr c))))
  ;; status 0 is not a failure
  (is (null (parse-error-output (make-invocation "wc") nil 0))))

(test grep-exit-code-translation
  "grep's wrapper knows exit 1 = \"no match\" is benign, exit >=2 is an error."
  (let ((g (make-invocation "grep" "foo")))
    (is (null (parse-error-output g nil 0)))        ; matched
    (is (null (parse-error-output g nil 1)))        ; no match: benign
    (let ((c (parse-error-output g (make-string-input-stream "err") 2)))
      (is (typep c 'command-failed))
      (is (= 2 (command-failed-status c))))))

(test command-failed-is-a-subtype-of-error
  (is (subtypep 'command-failed 'error)))

;;; ---------------------------------------------------------------------------
;;; unparse-input
;;; ---------------------------------------------------------------------------

(test unparse-input-default-writes-lines
  (is (string= (format nil "1~%two~%THREE~%")
               (with-output-to-string (s)
                 (unparse-input (make-invocation "cat") (list 1 "two" :three) s)))))

;;; ---------------------------------------------------------------------------
;;; find: pathnames, NUL-splitting, and the -print0 rewrite
;;; ---------------------------------------------------------------------------

(test find-yields-pathnames
  (let* ((inv (make-invocation "find" "/tmp"))
         (objs (seq-collect (parse-output inv (string-stream-of "/a" "/b/c")))))
    (is (= 2 (length objs)))
    (is (every #'pathnamep objs))
    (is (equal "/a" (namestring (first objs))))))

(test find-print0-splits-on-nul
  (let* ((inv (make-invocation "find" "/tmp" "-print0"))
         (data (format nil "/x~C/y~C/z" #\Nul #\Nul))   ; last record unterminated
         (objs (seq-collect (parse-output inv (make-string-input-stream data)))))
    (is-true (find-print0-p inv))
    (is (equal '("/x" "/y" "/z") (mapcar #'namestring objs)))))

(test find-rewrite-adds-print0
  (let* ((inv (make-invocation "find" "/tmp"))
         (rw (rewrite-invocation inv)))
    (is-false (find-print0-p inv))
    (is-true (find-print0-p rw))
    (is (member "-print0" (invocation-arguments rw) :test #'string=))
    ;; already -print0 -> returned unchanged
    (let ((inv2 (make-invocation "find" "/tmp" "-print0")))
      (is (eq inv2 (rewrite-invocation inv2))))))

(test rewrite-invocation-default-identity
  (let ((inv (make-invocation "cat" "f")))
    (is (eq inv (rewrite-invocation inv)))))

;;; ---------------------------------------------------------------------------
;;; cat / grep default line output; dialect scaffold
;;; ---------------------------------------------------------------------------

(test cat-and-grep-yield-lines
  (let ((cat (make-invocation "cat" "f"))
        (grep (make-invocation "grep" "foo")))
    (is (typep cat 'cat-invocation))
    (is (typep grep 'grep-invocation))
    (is (equal '("l1" "l2")
               (seq-collect (parse-output cat (string-stream-of "l1" "l2")))))
    (is (equal '("m1" "m2")
               (seq-collect (parse-output grep (string-stream-of "m1" "m2")))))))

(test command-dialect-scaffold
  (let ((inv (make-invocation "ls")))
    (is (eq :unknown (command-dialect inv)))
    (setf (invocation-dialect inv) :gnu)
    (is (eq :gnu (command-dialect inv)))))

;;; ===========================================================================
;;; Additional correctness coverage
;;; ===========================================================================

;;; --- lazy-sequence edges ---------------------------------------------------

(test empty-stream-yields-empty-sequence
  (is (null (seq-collect (parse-output (make-invocation "wc")
                                       (make-string-input-stream ""))))))

(test seq-next-on-empty-signals-end
  (multiple-value-bind (v more)
      (seq-next (parse-output (make-invocation "wc") (make-string-input-stream "")))
    (is (null v))
    (is-false more)))

(test seq-take-more-than-available-returns-all
  (let ((seq (parse-output (make-invocation "wc") (string-stream-of "a" "b" "c"))))
    (is (equal '("a" "b" "c") (seq-take 100 seq)))))

(test take-all-and-beyond-kills-producer
  (let ((seq (parse-output (make-invocation "wc") (string-stream-of "a" "b"))))
    (is (equal '("a" "b") (take 5 seq)))
    (is-false (sb-thread:thread-alive-p (object-seq-thread seq)))))

(test take-zero-kills-producer
  (let ((seq (spawn-object-seq
              (lambda (emit) (loop for i from 0 do (funcall emit i)))
              :capacity 2)))
    (is (null (take 0 seq)))
    (is-false (sb-thread:thread-alive-p (object-seq-thread seq)))))

(test producer-thread-finishes-after-collect
  (let ((seq (parse-output (make-invocation "wc") (string-stream-of "x" "y"))))
    (is (equal '("x" "y") (seq-collect seq)))
    (sb-thread:join-thread (object-seq-thread seq))       ; returns => it ended
    (is-false (sb-thread:thread-alive-p (object-seq-thread seq)))))

(test seq-close-after-drain-is-safe
  (let ((seq (parse-output (make-invocation "wc") (string-stream-of "a"))))
    (is (equal '("a") (seq-collect seq)))
    (finishes (seq-close seq))
    (finishes (seq-close seq))))                          ; idempotent

(test independent-sequences-do-not-interfere
  (let ((s1 (parse-output (make-invocation "wc") (string-stream-of "1" "2")))
        (s2 (parse-output (make-invocation "wc") (string-stream-of "a" "b"))))
    (is (equal "1" (seq-next s1)))
    (is (equal "a" (seq-next s2)))
    (is (equal "2" (seq-next s1)))
    (is (equal "b" (seq-next s2)))))

(test parse-output-is-lazy-and-backpressured
  "A backpressured producer runs at most ~capacity ahead of the consumer — an
eager implementation would race to the end."
  (let ((produced 0))
    (let ((seq (spawn-object-seq
                (lambda (emit)
                  (dotimes (i 100000) (funcall emit i) (incf produced)))
                :capacity 4)))
      (is (equal '(0) (seq-take 1 seq)))
      (sleep 0.1)
      (is (< produced 100))                              ; nowhere near 100000
      (seq-close seq))))

;;; --- shell-special propagation (SPEC §5) -----------------------------------

(test spawn-stage-thread-propagates-current-directory
  (let ((captured nil)
        (*current-directory* #P"/tmp/consh-specials-xyz/"))
    (sb-thread:join-thread
     (spawn-stage-thread (lambda () (setf captured *current-directory*))))
    (is (equal #P"/tmp/consh-specials-xyz/" captured))))

;;; --- registry / flag parsing edges -----------------------------------------

(test flag-present-p-detects-any
  (is-true (flag-present-p '("-a" "-b") "-b"))
  (is-true (flag-present-p '("-a" "--long") "-l" "--long"))
  (is-false (flag-present-p '("-a") "-x")))

(test split-flags-edges
  (multiple-value-bind (f o) (split-flags '())
    (is (null f)) (is (null o)))
  (multiple-value-bind (f o) (split-flags '("-a" "-b"))
    (is (equal '("-a" "-b") f)) (is (null o)))
  (multiple-value-bind (f o) (split-flags '("x" "y"))
    (is (null f)) (is (equal '("x" "y") o)))
  (multiple-value-bind (f o) (split-flags '("--"))
    (is (null f)) (is (null o))))

(test ls-flags-are-position-independent
  (let ((inv (make-invocation "ls" "/a" "-l" "/b")))
    (is-true (ls-long-p inv))
    (is (equal '("/a" "/b") (ls-paths inv)))))

(test custom-wrapper-needs-no-core-change
  "Registering a brand-new command dispatches to its class and parse-output."
  (is (typep (make-invocation "consh-kv") 'kv-invocation))
  (is (equal '(("a" . "1") ("b" . "2"))
             (seq-collect (parse-output (make-invocation "consh-kv")
                                        (string-stream-of "a=1" "b=2"))))))

(test custom-wrapper-restart-recovery
  (let ((objs (seq-collect (parse-output (make-invocation "consh-kv")
                                         (string-stream-of "a=1" "BAD" "c=3")
                                         :on-parse-error :use-raw-lines))))
    (is (equal "1" (cdr (first objs))))
    (is (string= "BAD" (second objs)))              ; malformed -> raw string
    (is (equal "3" (cdr (third objs))))))

;;; --- ls enrichment: current-directory, derived dir, mode, blanks -----------

(test ls-honors-current-directory-not-process-cwd
  "With paths defaulting to \".\", ls enriches entries of *current-directory*,
not the process working directory (consh never chdir's)."
  (let ((dir (make-temp-dir)))
    (with-open-file (s (merge-pathnames "here.txt" dir) :direction :output)
      (write-string "hi" s))
    (let ((*current-directory* dir))
      (let* ((inv (make-instance 'ls-invocation))        ; no :directory, paths = (".")
             (objs (seq-collect (parse-output inv (string-stream-of "here.txt")))))
        (is (= 1 (length objs)))
        (is (typep (first objs) 'file-info))
        (is (= 2 (file-size (first objs))))))))

(test ls-directory-derived-from-operand
  (let ((dir (make-temp-dir)))
    (is (equal (truename dir)
               (truename (ls-directory (make-invocation "ls" (namestring dir))))))))

(test ls-enriches-mode-uid-gid-for-file-and-dir
  (let ((dir (make-temp-dir)))
    (with-open-file (s (merge-pathnames "afile" dir) :direction :output)
      (write-string "z" s))
    (ensure-directories-exist (merge-pathnames "asub/" dir))
    (let* ((inv (make-instance 'ls-invocation :directory dir))
           (objs (seq-collect (parse-output inv (string-stream-of "afile" "asub"))))
           (f (find "afile" objs :key #'file-name :test #'string=))
           (d (find "asub"  objs :key #'file-name :test #'string=)))
      (is (= #o100000 (logand (file-mode f) #o170000)))   ; S_IFREG
      (is (= #o040000 (logand (file-mode d) #o170000)))   ; S_IFDIR
      (is (integerp (file-uid f)))
      (is (integerp (file-gid f)))
      (is (equal (uid-username (file-uid f)) (file-owner f)))
      (is (probe-file (file-path f))))))

(test ls-skips-blank-lines
  (let ((dir (make-temp-dir)))
    (with-open-file (s (merge-pathnames "x.txt" dir) :direction :output)
      (write-string "q" s))
    (let* ((inv (make-instance 'ls-invocation :directory dir))
           (objs (seq-collect
                  (parse-output inv (make-string-input-stream
                                     (format nil "x.txt~%~%x.txt~%"))))))
      (is (= 2 (length objs))))))                          ; the blank is ignored

;;; --- parse-error context + define-parser through the real producer ---------

(test parse-error-carries-command-raw-cause
  (let ((dir (make-temp-dir))
        (captured nil))
    (with-open-file (s (merge-pathnames "ok" dir) :direction :output)
      (write-string "k" s))
    (let ((inv (make-instance 'ls-invocation :directory dir)))
      (seq-collect (parse-output inv (string-stream-of "GONE")
                                 :on-parse-error
                                 (lambda (c) (setf captured c)
                                   (invoke-restart 'use-raw-lines))))
      (is (typep captured 'parse-error))
      (is (eq inv (parse-error-command captured)))
      (is (string= "GONE" (parse-error-raw captured)))
      (is (typep (parse-error-cause captured) 'ffi-error)))))

(test define-parser-policy-through-parse-output
  (let ((dir (make-temp-dir)))
    (with-open-file (s (merge-pathnames "ok" dir) :direction :output)
      (write-string "k" s))
    (let* ((inv (make-instance 'ls-invocation :directory dir))
           (objs (seq-collect
                  (parse-output inv (string-stream-of "GONE" "ok")
                                :on-parse-error
                                (lambda (c) (declare (ignore c))
                                  (invoke-restart 'define-parser
                                                  (lambda (raw) (list :missing raw))))))))
      (is (equal '(:missing "GONE") (first objs)))
      (is (typep (second objs) 'file-info)))))

;;; --- parse-error-output / command-failed extras ----------------------------

(test parse-error-output-non-integer-status
  (let ((c (parse-error-output (make-invocation "wc") nil :killed)))
    (is (typep c 'command-failed))
    (is (eq :killed (command-failed-status c)))))

(test command-failed-carries-command
  (let* ((inv (make-invocation "wc"))
         (c (parse-error-output inv (make-string-input-stream "e") 3)))
    (is (eq inv (command-failed-command c)))))

;;; --- unparse-input extras --------------------------------------------------

(test unparse-input-empty-list
  (is (string= "" (with-output-to-string (s)
                    (unparse-input (make-invocation "cat") '() s)))))

(test unparse-parse-round-trips-strings
  (let ((text (with-output-to-string (s)
                (unparse-input (make-invocation "cat")
                               '("alpha" "beta" "gamma") s))))
    (is (equal '("alpha" "beta" "gamma")
               (seq-collect (parse-output (make-invocation "wc")
                                          (make-string-input-stream text)))))))

;;; --- find extras -----------------------------------------------------------

(test find-empty-stream-yields-nothing
  (is (null (seq-collect (parse-output (make-invocation "find" "/tmp")
                                       (make-string-input-stream ""))))))

(test find-print0-terminated-has-no-empty-final-record
  (let* ((inv (make-invocation "find" "/tmp" "-print0"))
         (data (format nil "a~Cb~C" #\Nul #\Nul)))       ; properly terminated
    (is (equal '("a" "b")
               (mapcar #'namestring
                       (seq-collect (parse-output inv (make-string-input-stream data))))))))

(test find-print0-single-unterminated-record
  (let ((inv (make-invocation "find" "/tmp" "-print0")))
    (is (equal '("solo")
               (mapcar #'namestring
                       (seq-collect (parse-output inv (make-string-input-stream "solo"))))))))

(test find-rewrite-preserves-existing-args
  (let* ((inv (make-invocation "find" "/tmp" "-type" "f"))
         (rw (rewrite-invocation inv)))
    (is (equal '("/tmp" "-type" "f" "-print0") (invocation-arguments rw)))
    (is (string= "/tmp" (find-start rw)))))

;;; ===========================================================================
;;; git status --porcelain wrapper
;;; ===========================================================================

(defun %porcelain-stream (&rest lines)
  (make-string-input-stream
   (with-output-to-string (s) (dolist (l lines) (write-line l s)))))

(test git-parses-porcelain-into-status-objects
  (let* ((inv (make-invocation "git" "status"))
         (objs (seq-collect
                (parse-output inv (%porcelain-stream " M src/foo.lisp"
                                                     "?? new file.txt"
                                                     "A  staged.txt"
                                                     "R  old.txt -> new.txt")))))
    (is (= 4 (length objs)))
    (is (every (lambda (o) (typep o 'git-status)) objs))
    (destructuring-bind (modified untracked staged renamed) objs
      (is (string= " M" (git-status-code modified)))
      (is (string= "src/foo.lisp" (git-status-path modified)))
      (is (git-status-unstaged-p modified))
      (is (not (git-status-staged-p modified)))
      (is (git-status-untracked-p untracked))
      (is (string= "new file.txt" (git-status-path untracked)))   ; path may hold spaces
      (is (git-status-staged-p staged))
      (is (string= "new.txt" (git-status-path renamed)))
      (is (string= "old.txt" (git-status-orig-path renamed))))))

(test git-status-readers
  (let ((s (first (seq-collect (parse-output (make-invocation "git" "status")
                                             (%porcelain-stream "MM a.txt"))))))
    (is (char= #\M (git-status-index-char s)))       ; staged M
    (is (char= #\M (git-status-worktree-char s)))     ; and worktree M
    (is (git-status-staged-p s))
    (is (git-status-unstaged-p s))
    (is (not (git-status-untracked-p s)))))

(test git-malformed-status-line-signals
  (signals parse-error (consh::%parse-git-status-line (make-invocation "git" "status") "x")))

(test git-rewrite-adds-porcelain-only-for-status
  (is (equal '("status" "--porcelain")
             (invocation-arguments (rewrite-invocation (make-invocation "git" "status")))))
  ;; idempotent
  (is (equal '("status" "--porcelain")
             (invocation-arguments (rewrite-invocation (make-invocation "git" "status" "--porcelain")))))
  ;; other subcommands untouched
  (let ((inv (make-invocation "git" "log")))
    (is (eq inv (rewrite-invocation inv)))))

(test git-non-status-subcommand-yields-lines
  (let ((inv (make-invocation "git" "log")))
    (is (equal '("commit abc" "Author: x")
               (seq-collect (parse-output inv (%porcelain-stream "commit abc" "Author: x")))))))

(test git-status-real-repo-end-to-end
  "Init a real repo, add an untracked file, and let the wrapper rewrite to
--porcelain, run git, and parse the result."
  (let ((dir (make-temp-dir)))
    (let ((*current-directory* dir))
      (pipeline-collect (make-pipeline (list (external "git" "init" "-q"))))
      (with-open-file (s (merge-pathnames "hello.txt" dir) :direction :output)
        (write-string "x" s))
      (let* ((entries (pipeline-collect (make-pipeline (list (external "git" "status")))))
             (e (find "hello.txt" entries
                      :key (lambda (x) (and (typep x 'git-status) (git-status-path x)))
                      :test #'equal)))
        (is (typep e 'git-status))
        (is (git-status-untracked-p e))))))

;;; ===========================================================================
;;; ps wrapper: fixed columns -> process objects
;;; ===========================================================================

(test ps-parses-columns-into-process-objects
  (let* ((inv (rewrite-invocation (make-invocation "ps")))
         (input (format nil "90201 90198 mkennedy 160768 S+   claude --flag~%~
                             ~4T1     0 root       120 Ss   /sbin/launchd~%"))
         (objs (seq-collect (parse-output inv (make-string-input-stream input)))))
    (is (= 2 (length objs)))
    (is (every (lambda (o) (typep o 'ps-process)) objs))
    (destructuring-bind (a b) objs
      (is (= 90201 (ps-process-pid a)))
      (is (= 90198 (ps-process-ppid a)))
      (is (string= "mkennedy" (ps-process-user a)))
      (is (= 160768 (ps-process-rss a)))
      (is (string= "S+" (ps-process-state a)))
      (is (string= "claude --flag" (ps-process-command a)))   ; command keeps spaces
      (is (= 1 (ps-process-pid b)))
      (is (string= "/sbin/launchd" (ps-process-command b))))))

(test ps-rewrite-imposes-format-unless-user-gave-one
  (is (equal '("-o" "pid=,ppid=,user=,rss=,stat=,command=")
             (invocation-arguments (rewrite-invocation (make-invocation "ps")))))
  ;; preserves selection args, appends -o
  (is (equal '("-A" "-o" "pid=,ppid=,user=,rss=,stat=,command=")
             (invocation-arguments (rewrite-invocation (make-invocation "ps" "-A")))))
  ;; a user-supplied -o is left alone
  (let ((inv (make-invocation "ps" "-o" "pid,comm")))
    (is (eq inv (rewrite-invocation inv)))))

(test ps-user-format-yields-lines
  (let ((inv (make-invocation "ps" "-o" "pid,comm")))
    (is (equal '("100 bash" "200 vim")
               (seq-collect (parse-output inv (%porcelain-stream "100 bash" "200 vim")))))))

(test ps-malformed-line-signals
  (signals parse-error
    (consh::%parse-ps-line (make-invocation "ps") "only two")))

(test ps-real-finds-current-process
  "Run the real `ps`: the wrapper imposes its columns and yields ps-process
objects; our own SBCL process is among them."
  (let* ((mypid (sb-posix:getpid))
         (objs (pipeline-collect
                (make-pipeline (list (external "ps" "-p" (princ-to-string mypid))))))
         (me (find mypid objs
                   :key (lambda (x) (and (typep x 'ps-process) (ps-process-pid x))))))
    (is (typep me 'ps-process))
    (is (= mypid (ps-process-pid me)))
    (is (search "sbcl" (string-downcase (ps-process-command me))))))
