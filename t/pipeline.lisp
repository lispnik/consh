;;;; t/pipeline.lisp — Phase 4: pipeline compiler + executor.

(in-package #:consh/test)

(def-suite pipeline :in consh :description "Pipeline plumbing, fusion, executor.")
(in-suite pipeline)

;;; A shell fragment producing exactly LINES on stdout.
(defun emit-lines-cmd (&rest lines)
  (external "sh" "-c"
            (format nil "printf '~{~A\\n~}'" lines)))

;;; ===========================================================================
;;; Pipeline objects, the pipe macro, plumbing analysis
;;; ===========================================================================

(test pipe-builds-data-not-execution
  "pipe returns a pipeline object; nothing runs until run-pipeline."
  (let ((p (pipe (find "/tmp") (grep "foo"))))
    (is (typep p 'pipeline))
    (is (= 2 (length (pipeline-stages p))))
    (is (every #'external-stage-p (pipeline-stages p)))))

(test pipe-mixes-external-and-lisp-clauses
  (let ((p (pipe (cat "f") (:filter #'evenp) (:map #'1+))))
    (is (external-stage-p (first (pipeline-stages p))))
    (is (lisp-stage-p (second (pipeline-stages p))))
    (is (lisp-stage-p (third (pipeline-stages p))))))

(test adjacent-externals-form-one-kernel-piped-group
  "Two externals are one group joined by a kernel pipe — no Lisp boundary
between them (SPEC §3)."
  (let ((groups (pipeline-groups (pipe (find "/") (grep "x")))))
    (is (= 1 (length groups)))
    (is (eq :external (caar groups)))
    (is (= 2 (length (cdar groups)))))
  (let ((plan (pipeline-plan (pipe (find "/") (grep "x")))))
    (is (null (getf plan :boundaries)))                 ; no cross-Lisp boundary
    (is (equal '(:kernel-pipe)
               (getf (first (getf plan :groups)) :internal-links)))))

(test lisp-stages-fuse-into-one-group
  "Consecutive plain lisp stages fuse; an :expensive stage breaks fusion."
  (is (equal '(:external :lisp)
             (mapcar #'car (pipeline-groups (pipe (cat) (:filter #'evenp) (:map #'1+))))))
  ;; :expensive forces its own group
  (is (equal '(:external :lisp :lisp)
             (mapcar #'car
                     (pipeline-groups
                      (pipe (cat) (:filter #'evenp) (:map #'1+ :expensive t)))))))

(test plan-classifies-boundaries
  (let ((plan (pipeline-plan (pipe (cat) (:map #'identity) (grep "x")))))
    (is (equal '(:parse :unparse) (getf plan :boundaries)))
    (is (eq :stdin (getf plan :head)))
    (is (eq :parse (getf plan :tail)))))

(test describe-prints-a-plan
  (let ((text (with-output-to-string (s)
                (describe (pipe (find "/") (:map #'identity)) s))))
    (is (search "Pipeline of" text))
    (is (search "external" text))
    (is (search "boundary" text))))

;;; ===========================================================================
;;; Executing pure-external pipelines
;;; ===========================================================================

(test single-external-yields-lines
  (is (equal '("a" "b" "c")
             (pipeline-collect (make-pipeline (list (emit-lines-cmd "a" "b" "c")))))))

(test external-to-external-filters
  ;; grep is the tail: rewritten to -n, it yields grep-match objects
  (is (equal '("foo" "foobar")
             (mapcar #'grep-match-text
                     (pipeline-collect
                      (make-pipeline (list (emit-lines-cmd "foo" "bar" "foobar")
                                           (external "grep" "foo"))))))))

(test external-to-external-uses-one-pipe-no-lisp-traffic
  "Acceptance: an external→external pair costs exactly one parse pump (the tail).
If bytes were routed through Lisp between them there would be a second pump."
  (let ((r (run-pipeline (make-pipeline (list (emit-lines-cmd "foo" "bar" "foobar")
                                              (external "grep" "foo"))))))
    (unwind-protect
         (progn
           (is (equal '("foo" "foobar")
                      (mapcar #'grep-match-text (seq-collect (pipeline-result-seq r)))))
           (is (= 1 (pipeline-result-pump-count r)))    ; only the tail parse
           (is (= 2 (length (pipeline-result-processes r)))))
      (consh::%teardown (pipeline-result-state r)))))

(test three-external-stages
  (is (equal '("FOOBAR")
             (pipeline-collect
              (make-pipeline (list (emit-lines-cmd "foo" "bar" "foobar")
                                   (external "grep" "foobar")
                                   (external "tr" "a-z" "A-Z")))))))

;;; ===========================================================================
;;; Cancellation: take kills the producer AND the processes (SPEC §2/§4)
;;; ===========================================================================

(test take-terminates-and-kills-external
  "Acceptance: taking a prefix of an unbounded pipeline stops promptly and the
external is dead afterward — no zombies remain."
  (stop-reaper)
  (ensure-reaper)
  (unwind-protect
       (progn
         (is (equal '("y" "y" "y" "y" "y")
                    (take 5 (make-pipeline (list (external "yes"))))))
         ;; everything reaped: waitpid(-1) -> ECHILD
         (stop-reaper)
         (multiple-value-bind (pid raw errno) (c-waitpid -1 +wnohang+)
           (declare (ignore raw))
           (is (= -1 pid))
           (is (= +echild+ errno))))
    (ensure-reaper)))

(test yes-head-does-not-hang
  "Acceptance: a fast producer into an early-exiting consumer (yes | head)
completes via SIGPIPE without hanging."
  (is (= 10 (length (pipeline-collect
                     (make-pipeline (list (external "yes") (external "head"))))))))

(test interrupt-during-foreground-kills-processes
  "C-c during a foreground command: an interrupt in the collecting thread unwinds
through pipeline-collect's teardown, killing and reaping the processes (this is
how the REPL's Ctrl-C handling stops a running job)."
  (stop-reaper)
  (ensure-reaper)
  (unwind-protect
       (let ((th (sb-thread:make-thread
                  (lambda ()
                    (handler-case
                        (pipeline-collect
                         (make-pipeline (list (external "sh" "-c" "sleep 30"))))
                      (serious-condition () :interrupted))))))
         (sleep 0.3)
         (sb-thread:interrupt-thread th (lambda () (error "simulated C-c")))
         (sb-thread:join-thread th)
         (sleep 0.15)
         (stop-reaper)
         (multiple-value-bind (pid raw errno) (c-waitpid -1 +wnohang+)
           (declare (ignore raw))
           (is (= -1 pid))
           (is (= +echild+ errno))))                   ; the sleep was killed + reaped
    (ensure-reaper)))

;;; ===========================================================================
;;; stderr draining (no deadlock)
;;; ===========================================================================

(test stderr-heavy-does-not-deadlock
  "Acceptance: a process flooding stderr while emitting little stdout completes
because stderr is drained concurrently."
  (is (equal '("done")
             (pipeline-collect
              (make-pipeline
               (list (external "sh" "-c"
                               "i=0; while [ $i -lt 20000 ]; do echo errline$i >&2; i=$((i+1)); done; echo done")))))))

(test stderr-captured-for-failure
  (let ((r (run-pipeline (make-pipeline
                          (list (external "sh" "-c" "echo oops 1>&2; exit 4")))
                         :on-failure :collect)))
    (handler-case (progn (seq-collect (pipeline-result-seq r))
                         (dolist (p (pipeline-result-processes r)) (wait-process p :timeout 5)))
      (error () nil))
    (let ((failure (consh::%first-failure r)))
      (is (typep failure 'pipeline-failed))
      (is (search "oops" (command-failed-stderr failure))))
    (consh::%teardown (pipeline-result-state r))))

(test stderr-capture-is-bounded
  "A stderr flood (>100 KB) is fully drained (no deadlock) but only a bounded
amount is retained for the failure condition."
  (let ((r (run-pipeline
            (make-pipeline
             (list (external "sh" "-c"
                             "i=0; while [ $i -lt 10000 ]; do echo errline$i 1>&2; i=$((i+1)); done; exit 2")))
            :on-failure :collect)))
    (pipeline-collect r)                                  ; completes -> no deadlock
    (let ((stderr (command-failed-stderr (consh::%first-failure r))))
      (is (<= (length stderr) (+ consh::*stderr-capture-limit* 64)))
      (is (search "truncated" stderr)))))

;;; ===========================================================================
;;; :on-failure modes and restart-stage
;;; ===========================================================================

(test signal-mode-raises-typed-condition-naming-stage
  "Acceptance: a failing middle stage under :signal delivers a typed condition
naming that stage."
  (handler-case
      (progn
        (pipeline-collect
         (run-pipeline (make-pipeline (list (emit-lines-cmd "hi")
                                            (external "sh" "-c" "cat >/dev/null; exit 3")
                                            (external "cat")))
                       :on-failure :signal))
        (fail "expected pipeline-failed"))
    (pipeline-failed (c)
      (is (= 1 (pipeline-failed-stage-index c)))         ; the middle stage
      (is (string= "sh" (invocation-program (command-failed-command c))))
      (is (= 3 (command-failed-status c))))))

(test collect-mode-does-not-signal
  (is (null (pipeline-collect
             (run-pipeline (make-pipeline (list (external "sh" "-c" "exit 7")))
                           :on-failure :collect)))))

(test grep-no-match-is-benign-under-signal
  "grep exiting 1 (no match) must not raise, thanks to its parse-error-output."
  (is (null (pipeline-collect
             (run-pipeline (make-pipeline (list (emit-lines-cmd "apple")
                                                (external "grep" "zzz")))
                           :on-failure :signal)))))

(test restart-stage-reruns-corrected-pipeline
  (is (equal '("recovered")
             (handler-bind
                 ((pipeline-failed
                    (lambda (c) (declare (ignore c))
                      (invoke-restart 'restart-stage
                                      (make-pipeline (list (emit-lines-cmd "recovered")))))))
               (pipeline-collect
                (run-pipeline (make-pipeline (list (external "sh" "-c" "exit 5")))
                              :on-failure :signal))))))

;;; ===========================================================================
;;; Lisp stages: map / filter / mapcat, fusion, and boundaries
;;; ===========================================================================

(test external-to-lisp-map
  (is (equal '("A" "B" "C")
             (pipeline-collect
              (make-pipeline (list (emit-lines-cmd "a" "b" "c")
                                   (map-stage #'string-upcase)))))))

(test fused-filter-then-map-runs-in-one-worker
  "The two lisp stages fuse: one worker thread, no channel between them."
  (let ((r (run-pipeline (make-pipeline
                          (list (emit-lines-cmd "1" "2" "3" "4")
                                (filter-stage (lambda (s) (evenp (parse-integer s))))
                                (map-stage (lambda (s) (* 10 (parse-integer s)))))))))
    (unwind-protect
         (progn
           (is (equal '(20 40) (seq-collect (pipeline-result-seq r))))
           ;; one external group (parse pump) + one fused lisp worker
           (is (= 1 (length (consh::run-state-worker-threads (pipeline-result-state r))))))
      (consh::%teardown (pipeline-result-state r)))))

(test mapcat-stage-expands
  (is (equal '("a" "a" "b" "b")
             (pipeline-collect
              (make-pipeline (list (emit-lines-cmd "a" "b")
                                   (mapcat-stage (lambda (s) (list s s)))))))))

(test lisp-to-external-via-input
  "A Lisp object list is unparsed into an external's stdin; grep (rewritten to
-n) hands the matches back as grep-match objects."
  (let ((matches (pipeline-collect
                  (run-pipeline (make-pipeline (list (external "grep" "o")))
                                :input (list "foo" "bar" "boo")))))
    (is (every (lambda (m) (typep m 'grep-match)) matches))
    (is (equal '("foo" "boo") (mapcar #'grep-match-text matches)))
    (is (equal '(1 3) (mapcar #'grep-match-line-number matches)))))   ; input lines

(test lisp-only-pipeline-with-input
  (is (equal '(2 4 6)
             (pipeline-collect
              (run-pipeline (make-pipeline (list (filter-stage #'evenp)
                                                 (map-stage #'identity)))
                            :input (list 1 2 3 4 5 6))))))

(test expensive-stage-splits-into-its-own-worker
  (let ((r (run-pipeline (make-pipeline
                          (list (emit-lines-cmd "1" "2")
                                (map-stage (lambda (s) (parse-integer s)))
                                (map-stage #'1+ :expensive t))))))
    (unwind-protect
         (progn
           (is (equal '(2 3) (seq-collect (pipeline-result-seq r))))
           (is (= 2 (length (consh::run-state-worker-threads (pipeline-result-state r))))))
      (consh::%teardown (pipeline-result-state r)))))

;;; ===========================================================================
;;; pipeline-result metadata
;;; ===========================================================================

(test result-records-processes-and-timing
  (let ((r (run-pipeline (make-pipeline (list (emit-lines-cmd "a") (external "cat"))))))
    (pipeline-collect r)
    (is (= 2 (length (pipeline-result-processes r))))
    (is (every (lambda (p) (eq :exited (process-status p)))
               (pipeline-result-processes r)))
    (is (integerp (pipeline-result-elapsed r)))))

(test enriched-objects-flow-through-a-pipeline
  "End-to-end with real ls: file objects survive an external→lisp boundary."
  (let ((dir (make-temp-dir)))
    (with-open-file (s (merge-pathnames "big.txt" dir) :direction :output)
      (write-string "0123456789" s))                    ; 10 bytes
    (with-open-file (s (merge-pathnames "small.txt" dir) :direction :output)
      (write-string "x" s))                             ; 1 byte
    (let* ((*current-directory* dir)
           (result (pipeline-collect
                    (make-pipeline
                     (list (external (make-invocation "ls" (namestring dir)))
                           (filter-stage (lambda (f) (and (typep f 'file-info)
                                                          (> (file-size f) 5)))))))))
      (is (= 1 (length result)))
      (is (string= "big.txt" (file-name (first result)))))))

;;; ===========================================================================
;;; Large-volume data integrity across each boundary type (>64 KiB)
;;; ===========================================================================

(test high-volume-external-to-external-preserves-order-and-count
  (let ((out (pipeline-collect
              (make-pipeline (list (external "sh" "-c" "seq 1 20000")
                                   (external "cat"))))))
    (is (= 20000 (length out)))
    (is (string= "1" (first out)))
    (is (string= "20000" (car (last out))))))

(test high-volume-external-to-lisp-preserves-data
  (let ((sum (reduce #'+
                     (pipeline-collect
                      (make-pipeline (list (external "sh" "-c" "seq 1 20000")
                                           (map-stage #'parse-integer)))))))
    (is (= (/ (* 20000 20001) 2) sum))))

(test high-volume-lisp-to-external-preserves-order-and-count
  (let ((out (pipeline-collect
              (run-pipeline (make-pipeline (list (external "cat")))
                            :input (loop for i from 1 to 10000
                                         collect (princ-to-string i))))))
    (is (= 10000 (length out)))
    (is (string= "1" (first out)))
    (is (string= "10000" (car (last out))))))

;;; ===========================================================================
;;; Alternating boundaries and round-trips
;;; ===========================================================================

(test alternating-external-lisp-external-lisp
  "external -> lisp -> external -> lisp: objects are unparsed to text mid-pipe
and re-parsed, surviving intact."
  (is (equal '("a" "b")
             (pipeline-collect
              (make-pipeline (list (emit-lines-cmd "a" "b")
                                   (map-stage #'string-upcase)
                                   (external "cat")
                                   (map-stage #'string-downcase)))))))

(test lisp-to-external-exact-passthrough
  (is (equal '("one" "two" "three")
             (pipeline-collect
              (run-pipeline (make-pipeline (list (external "cat")))
                            :input '("one" "two" "three"))))))

(test clos-objects-keep-identity-through-lisp-pipeline
  (let* ((a (list :obj 1)) (b (list :obj 2))
         (out (pipeline-collect
               (run-pipeline (make-pipeline (list (filter-stage (constantly t))))
                             :input (list a b)))))
    (is (eq a (first out)))
    (is (eq b (second out)))))

;;; ===========================================================================
;;; Cancellation through a lisp worker
;;; ===========================================================================

(test take-through-lisp-worker-kills-external
  "Cancelling a prefix of external -> lisp cascades: the external is killed and
reaped, the worker stops."
  (stop-reaper)
  (ensure-reaper)
  (unwind-protect
       (progn
         (is (equal '("Y" "Y" "Y")
                    (take 3 (make-pipeline (list (external "yes")
                                                 (map-stage #'string-upcase))))))
         (stop-reaper)
         (multiple-value-bind (pid raw errno) (c-waitpid -1 +wnohang+)
           (declare (ignore raw))
           (is (= -1 pid))
           (is (= +echild+ errno))))
    (ensure-reaper)))

;;; ===========================================================================
;;; Failure-index edge cases and signal deaths
;;; ===========================================================================

(defun %run-and-catch-failure (pipeline)
  "Run PIPELINE under :signal and return the pipeline-failed condition, or NIL."
  (handler-case (progn (pipeline-collect (run-pipeline pipeline :on-failure :signal))
                       nil)
    (pipeline-failed (c) c)))

(test failure-names-first-stage
  (let ((c (%run-and-catch-failure
            (make-pipeline (list (external "sh" "-c" "exit 2") (external "cat"))))))
    (is (typep c 'pipeline-failed))
    (is (= 0 (pipeline-failed-stage-index c)))
    (is (= 2 (command-failed-status c)))))

(test failure-names-last-stage
  (let ((c (%run-and-catch-failure
            (make-pipeline (list (emit-lines-cmd "hi")
                                 (external "sh" "-c" "cat >/dev/null; exit 4"))))))
    (is (= 1 (pipeline-failed-stage-index c)))
    (is (= 4 (command-failed-status c)))))

(test multiple-failures-report-the-first
  (let ((c (%run-and-catch-failure
            (make-pipeline (list (external "sh" "-c" "exit 5")
                                 (external "sh" "-c" "cat >/dev/null; exit 6"))))))
    (is (= 0 (pipeline-failed-stage-index c)))
    (is (= 5 (command-failed-status c)))))

(test death-by-signal-is-a-failure-under-signal
  "A stage killed by a signal is reported as failed (status 128+signal)."
  (let ((c (%run-and-catch-failure
            (make-pipeline (list (external "sh" "-c" "kill -TERM $$; sleep 1"))))))
    (is (typep c 'pipeline-failed))
    (is (= (+ 128 +sigterm+) (command-failed-status c)))))

(test nonexistent-command-signals-and-cleans-up
  "A spawn failure mid-pipeline raises spawn-error and kills whatever already
started (no leaked processes)."
  (stop-reaper)
  (ensure-reaper)
  (unwind-protect
       (progn
         (signals spawn-error
           (run-pipeline (make-pipeline (list (external "yes")
                                              (external "consh-nope-not-a-cmd")))))
         (sleep 0.1)
         (stop-reaper)
         (multiple-value-bind (pid raw errno) (c-waitpid -1 +wnohang+)
           (declare (ignore raw))
           (is (= -1 pid))
           (is (= +echild+ errno))))
    (ensure-reaper)))

;;; ===========================================================================
;;; Degenerate pipelines
;;; ===========================================================================

(test empty-output-pipelines
  (is (null (pipeline-collect (make-pipeline (list (external "true"))))))
  (is (null (pipeline-collect (make-pipeline (list (emit-lines-cmd "a")
                                                   (external "grep" "zzz")))))))

(test lisp-only-empty-input
  (is (null (pipeline-collect
             (run-pipeline (make-pipeline (list (map-stage #'1+))) :input '())))))

(test make-pipeline-requires-a-stage
  (signals error (make-pipeline '()))
  (signals error (make-pipeline nil)))

;;; ===========================================================================
;;; :collect exposes per-stage exit status
;;; ===========================================================================

(test collect-exposes-exit-codes
  (let ((r (run-pipeline (make-pipeline (list (external "sh" "-c" "exit 3")))
                         :on-failure :collect)))
    (is (null (pipeline-collect r)))                        ; no signal
    (is (= 3 (process-exit-code (first (pipeline-result-processes r)))))))

;;; ===========================================================================
;;; Plan details for the remaining boundary/head/tail shapes
;;; ===========================================================================

(test plan-channel-boundary-between-split-lisp-groups
  (let ((plan (pipeline-plan (pipe (cat) (:map #'identity :expensive t) (:map #'identity)))))
    (is (equal '(:parse :channel) (getf plan :boundaries)))))

(test plan-source-head-and-channel-tail-for-lisp-ends
  (let ((plan (pipeline-plan (pipe (:map #'identity)))))
    (is (eq :source (getf plan :head)))
    (is (eq :channel (getf plan :tail))))
  (let ((plan (pipeline-plan (pipe (cat) (:map #'identity)))))
    (is (eq :stdin (getf plan :head)))
    (is (eq :channel (getf plan :tail)))))

(test plan-three-externals-have-two-kernel-links
  (let ((plan (pipeline-plan (pipe (cat) (grep "x") (tr "a" "b")))))
    (is (null (getf plan :boundaries)))
    (is (equal '(:kernel-pipe :kernel-pipe)
               (getf (first (getf plan :groups)) :internal-links)))))

;;; ===========================================================================
;;; Running many pipelines does not leak fds/threads
;;; ===========================================================================

(test many-sequential-pipelines-do-not-leak
  (dotimes (i 40)
    (is (equal '("x") (pipeline-collect (make-pipeline (list (emit-lines-cmd "x")))))))
  ;; still healthy afterward
  (is (equal '("done")
             (pipeline-collect (make-pipeline (list (emit-lines-cmd "done")))))))

;;; ===========================================================================
;;; run-and-parse rewrite is applied to the run's tail (SPEC §2)
;;; ===========================================================================

(test find-pipeline-yields-enriched-file-objects
  (let ((dir (make-temp-dir)))
    (with-open-file (s (merge-pathnames "one.txt" dir) :direction :output)
      (write-string "x" s))
    (let ((objs (pipeline-collect
                 (make-pipeline (list (external "find" (namestring dir) "-type" "f"))))))
      (is (every (lambda (o) (typep o 'file-info)) objs))
      (is (member "one.txt" objs
                  :key (lambda (o) (file-namestring (file-path o))) :test #'string=)))))

(test executor-rewrites-tail-to-print0
  "The executor applies rewrite-invocation to the run's tail: `find` gets
-print0, so a filename containing a newline stays a SINGLE pathname.  Newline
line-splitting (no rewrite) would return it as two bogus records."
  (let* ((dir (make-temp-dir))
         (weird (merge-pathnames
                 (make-pathname :name (format nil "a~Cb" #\Newline) :type "txt") dir)))
    (with-open-file (s weird :direction :output) (write-string "x" s))
    (let ((objs (pipeline-collect
                 (make-pipeline (list (external "find" (namestring dir) "-type" "f"))))))
      (is (= 1 (length objs)))                          ; NUL-delimited: one record
      ;; the newline-named file exists, so it enriches; the point is it is ONE
      ;; record, not two bogus ones from newline-splitting
      (is (typep (first objs) 'file-info))
      (is (search (format nil "a~Cb" #\Newline)
                  (file-namestring (file-path (first objs))))))))

(test intermediate-external-is-not-rewritten
  "An intermediate external keeps normal (newline) output so the next external
consumes it correctly: find | grep still works (find is not -print0'd here)."
  (let ((dir (make-temp-dir)))
    (with-open-file (s (merge-pathnames "keep.log" dir) :direction :output)
      (write-string "x" s))
    (with-open-file (s (merge-pathnames "skip.txt" dir) :direction :output)
      (write-string "x" s))
    (let ((matches (pipeline-collect
                    (make-pipeline (list (external "find" (namestring dir) "-type" "f")
                                         (external "grep" "log"))))))
      (is (= 1 (length matches)))
      ;; grep is the tail: rewritten to -n, it yields a grep-match object
      (is (typep (first matches) 'grep-match))
      (is (search "keep.log" (grep-match-text (first matches)))))))

;;; ===========================================================================
;;; Imperative stages: emit-stage (transform) and generator-stage (source)
;;; ===========================================================================

(test emit-stage-expands-one-to-many
  (is (equal '("a" "a" "b" "b")
             (pipeline-collect
              (make-pipeline (list (emit-lines-cmd "a" "b")
                                   (emit-stage (lambda (x emit)
                                                 (funcall emit x)
                                                 (funcall emit x)))))))))

(test emit-stage-can-emit-nothing
  "Emitting zero times for some inputs makes emit-stage a filter."
  (is (equal '("2" "4")
             (pipeline-collect
              (make-pipeline (list (emit-lines-cmd "1" "2" "3" "4")
                                   (emit-stage (lambda (x emit)
                                                 (when (evenp (parse-integer x))
                                                   (funcall emit x))))))))))

(test emit-stage-is-stateful
  "The imperative sibling of map/filter: emit a value derived from accumulated
state (a running total) — which map/filter/mapcat cannot express."
  (let ((sum 0))
    (is (equal '(1 3 6 10 15)
               (pipeline-collect
                (make-pipeline (list (external "seq" "1" "5")
                                     (emit-stage (lambda (n emit)
                                                   (funcall emit (incf sum (parse-integer n)))))))))))
  ;; windowing / lookahead across inputs: emit each ascending adjacent pair
  (let ((prev nil))
    (is (equal '((1 . 4) (2 . 9))
               (pipeline-collect
                (run-pipeline
                 (make-pipeline
                  (list (emit-stage (lambda (x emit)
                                      (when (and prev (> x prev)) (funcall emit (cons prev x)))
                                      (setf prev x)))))
                 :input '(1 4 2 9 3)))))))

(test emit-stage-fuses-with-adjacent-transducers
  (let ((r (run-pipeline (make-pipeline
                          (list (emit-lines-cmd "1" "2" "3")
                                (map-stage #'parse-integer)
                                (emit-stage (lambda (n emit) (funcall emit (* n 10))))
                                (filter-stage (lambda (n) (> n 15))))))))
    (unwind-protect
         (progn
           (is (equal '(20 30) (seq-collect (pipeline-result-seq r))))
           ;; the three lisp stages fuse into ONE worker
           (is (= 1 (length (consh::run-state-worker-threads (pipeline-result-state r))))))
      (consh::%teardown (pipeline-result-state r)))))

(test emit-stage-via-pipe-macro
  (is (equal '("A" "B")
             (pipeline-collect
              (pipe (:generate (emit) (dolist (w '("a" "STOP" "b")) (funcall emit w)))
                    (:emit (w emit) (unless (string= w "STOP")
                                      (funcall emit (string-upcase w)))))))))

(test generator-stage-is-a-source
  (is (equal '(0 1 4 9 16)
             (pipeline-collect
              (make-pipeline (list (generator-stage
                                    (lambda (emit) (dotimes (i 5) (funcall emit (* i i)))))))))))

(test generator-heads-its-own-fused-group
  (let ((r (run-pipeline (make-pipeline
                          (list (generator-stage (lambda (emit) (dotimes (i 6) (funcall emit i))))
                                (filter-stage #'evenp)
                                (map-stage #'1+))))))
    (unwind-protect
         (progn
           (is (equal '(1 3 5) (seq-collect (pipeline-result-seq r))))
           (is (= 1 (length (consh::run-state-worker-threads (pipeline-result-state r))))))
      (consh::%teardown (pipeline-result-state r))))
  ;; grouping: generator + transducers = a single :lisp group
  (is (equal '(:lisp)
             (mapcar #'car
                     (pipeline-groups
                      (pipe (:generate (emit) (funcall emit 1)) (:map #'1+) (:filter #'oddp)))))))

(test generator-feeds-an-external
  "A Lisp generator source can feed an external's stdin (lisp -> external)."
  (is (equal '("alpha" "beta")
             (pipeline-collect
              (make-pipeline (list (generator-stage
                                    (lambda (emit) (funcall emit "alpha") (funcall emit "beta")))
                                   (external "cat")))))))

(test take-from-infinite-generator-terminates
  "Backpressure + cancellation apply to a generator: taking a prefix stops it."
  (let ((seq (run-pipeline (make-pipeline
                            (list (generator-stage
                                   (lambda (emit) (loop for i from 0 do (funcall emit i)))))))))
    (is (equal '(0 1 2 3 4) (take 5 seq)))))
