;;;; t/jobs.lisp — Phase 5: jobs, fg/bg, C-z, job events, debug-job.

(in-package #:consh/test)

(def-suite jobs :in consh :description "Job control and cross-thread conditions.")
(in-suite jobs)

(defun poll-until (predicate &key (timeout 5.0) (interval 0.02))
  "Busy-wait (politely) until PREDICATE is true or TIMEOUT elapses."
  (loop repeat (ceiling timeout interval)
        when (funcall predicate) do (return t)
        do (sleep interval)
        finally (return (funcall predicate))))

;;; A source external emitting exactly LINES.
(defun lines-job-cmd (&rest lines)
  (external "sh" "-c" (format nil "printf '~{~A\\n~}'" lines)))

;;; A "command" whose bytes are real (printed by sh) but whose parse-output is
;;; kv's: it enriches "k=v" lines into conses and signals PARSE-ERROR on a line
;;; without '=' — used to force a parse-error inside a pipeline's parse worker.
(defun kv-source (text)
  (external (make-instance 'kv-invocation :program "sh"
                           :arguments (list "-c" (format nil "printf '~A'" text)))))

;;; ===========================================================================
;;; Job registry and lifecycle
;;; ===========================================================================

(test run-job-registers-and-numbers
  (let ((j (run-job (make-pipeline (list (lines-job-cmd "a"))) :background t)))
    (is (typep j 'job))
    (is (integerp (job-id j)))
    (is (eq j (find-job (job-id j))))
    (is (member j (all-jobs)))
    (fg j)))

(test job-ids-increase
  (let ((a (run-job (make-pipeline (list (lines-job-cmd "a"))) :background t))
        (b (run-job (make-pipeline (list (lines-job-cmd "b"))) :background t)))
    (is (< (job-id a) (job-id b)))
    (fg a) (fg b)))

(test job-reaches-done-state
  (let ((j (run-job (make-pipeline (list (lines-job-cmd "x"))) :background t)))
    (is (equal '("x") (fg j)))
    (is (eq :done (job-state j)))
    (is (job-complete-p j))))

;;; ===========================================================================
;;; Acceptance: background a pipeline, foreground it, output intact
;;; ===========================================================================

(test background-then-foreground-output-intact
  (let ((j (run-job (make-pipeline (list (lines-job-cmd "a" "b" "c"))) :background t)))
    (is (job-background-p j))
    (is (equal '("a" "b" "c") (fg j)))))

(test wait-job-returns-output
  (multiple-value-bind (out ok)
      (wait-job (run-job (make-pipeline (list (lines-job-cmd "one" "two"))) :background t))
    (is-true ok)
    (is (equal '("one" "two") out))))

(test background-job-with-lisp-stage
  (let ((j (run-job (make-pipeline (list (lines-job-cmd "a" "b")
                                         (map-stage #'string-upcase)))
                    :background t)))
    (is (equal '("A" "B") (fg j)))))

;;; ===========================================================================
;;; Acceptance: C-z stops the job (external + Lisp workers), continue resumes
;;; ===========================================================================

(test c-z-stops-external-process
  (let ((j (run-job (make-pipeline
                     (list (external "sh" "-c" "sleep 0.4; printf 'x\\ny\\n'")))
                    :background t)))
    (sleep 0.1)
    (stop-job j)                                     ; C-z
    (is (eq :stopped (job-state j)))
    ;; the reaper observes the external as actually stopped
    (is-true (poll-until (lambda ()
                           (eq :stopped (process-status
                                         (first (pipeline-result-processes (job-result j))))))))
    (is-false (job-complete-p j))                    ; frozen, not finished
    (continue-job j)
    (is (equal '("x" "y") (fg j)))))                 ; output intact after resume

(test c-z-parks-lisp-worker-then-resumes
  "A pipeline with a lisp stage freezes on C-z (the worker parks at its channel
op) and produces intact output after continue."
  (let ((j (run-job (make-pipeline
                     (list (external "sh" "-c" "sleep 0.3; printf 'a\\nb\\n'")
                           (map-stage #'string-upcase)))
                    :background t)))
    (sleep 0.1)
    (stop-job j)
    (is (eq :stopped (job-state j)))
    (sleep 0.2)
    (is-false (job-complete-p j))                    ; still frozen
    (continue-job j)
    (is (equal '("A" "B") (fg j)))))

(test fg-continues-a-stopped-job
  (let ((j (run-job (make-pipeline (list (external "sh" "-c" "sleep 0.2; printf 'z\\n'")))
                    :background t)))
    (sleep 0.05)
    (stop-job j)
    (is (eq :stopped (job-state j)))
    (is (equal '("z") (fg j)))                       ; fg resumes then waits
    (is (eq :done (job-state j)))))

;;; ===========================================================================
;;; Acceptance: parse-error parks the job, surfaces an event; debug-job resumes
;;; ===========================================================================

(test parse-error-parks-job-and-posts-event
  (let ((j (run-job (make-pipeline (list (kv-source "a=1\\nBAD\\nc=3\\n")))
                    :on-parse-error :error :background t)))
    (is-true (poll-until (lambda () (eq :parked (job-state j)))))
    (is (eq :parked (job-state j)))
    (is-false (job-complete-p j))                    ; the whole line is frozen
    (is (some (lambda (e) (search "parked" e)) (job-events j)))
    ;; the parked worker offers the record-level restarts, live
    (is (member 'use-raw-lines (job-restarts j)))
    (is (member 'define-parser (job-restarts j)))
    (kill-job j)))

(test debug-job-use-raw-lines-resumes-end-to-end
  "debug-job invokes a live restart in the parked worker's own context; the
frozen line resumes and completes with the recovered record."
  (let ((j (run-job (make-pipeline (list (kv-source "a=1\\nBAD\\nc=3\\n")))
                    :on-parse-error :error :background t)))
    (is-true (poll-until (lambda () (eq :parked (job-state j)))))
    (debug-job j :restart 'use-raw-lines)            ; recover the bad record
    (is (equal (list (cons "a" "1") "BAD" (cons "c" "3")) (fg j)))
    (is (eq :done (job-state j)))))

(test debug-job-define-parser-supplies-a-value
  (let ((j (run-job (make-pipeline (list (kv-source "a=1\\nBAD\\n")))
                    :on-parse-error :error :background t)))
    (is-true (poll-until (lambda () (eq :parked (job-state j)))))
    (debug-job j :restart 'define-parser
               :args (list (lambda (raw) (list :fixed raw))))
    (is (equal (list (cons "a" "1") (list :fixed "BAD")) (fg j)))))

(test lisp-worker-error-parks-and-skip-object-resumes
  "An error in a lisp stage parks the worker; debug-job with skip-object drops
the offending object and the pipeline finishes."
  (let ((j (run-job (make-pipeline
                     (list (lines-job-cmd "a" "BAD" "c")
                           (map-stage (lambda (s)
                                        (if (string= s "BAD")
                                            (error "boom on ~A" s)
                                            (string-upcase s))))))
                    :background t)))
    (is-true (poll-until (lambda () (eq :parked (job-state j)))))
    (is (member 'skip-object (job-restarts j)))
    (debug-job j :restart 'skip-object)
    (is (equal '("A" "C") (fg j)))))                 ; BAD skipped

(test resume-job-declines-and-line-ends
  "resume-job lets the condition propagate: the worker dies, so the line ends
with whatever was produced before the error."
  (let ((j (run-job (make-pipeline (list (kv-source "a=1\\nBAD\\nc=3\\n")))
                    :on-parse-error :error :background t)))
    (is-true (poll-until (lambda () (eq :parked (job-state j)))))
    (resume-job j)                                   ; decline -> propagate
    (is (equal (list (cons "a" "1")) (fg j)))))      ; only what preceded BAD

;;; ===========================================================================
;;; :on-failure under a job; kill-job
;;; ===========================================================================

(test job-signal-failure-raises-on-wait
  (let ((j (run-job (make-pipeline (list (external "sh" "-c" "exit 4")))
                    :on-failure :signal :background t)))
    (signals pipeline-failed (fg j))))

(test job-collect-failure-does-not-raise
  (let ((j (run-job (make-pipeline (list (external "sh" "-c" "exit 4")))
                    :on-failure :collect :background t)))
    (is (null (fg j)))
    (is (= 4 (process-exit-code (first (pipeline-result-processes (job-result j))))))))

(test kill-job-terminates-running-external
  (let ((j (run-job (make-pipeline (list (external "yes"))) :background t)))
    (sleep 0.05)
    (kill-job j)
    (is (eq :done (job-state j)))
    (is-true (poll-until (lambda ()
                           (process-exited-p
                            (first (pipeline-result-processes (job-result j)))))))))

;;; ===========================================================================
;;; Job events queue
;;; ===========================================================================

(test global-event-queue-drains
  (let ((j (run-job (make-pipeline (list (lines-job-cmd "a"))) :background t)))
    (fg j)
    (let ((events (take-job-events)))
      (is (some (lambda (e) (search "started" e)) events))
      (is (some (lambda (e) (search "done" e)) events)))
    ;; draining clears the queue
    (is (null (take-job-events)))))

(test job-events-are-ordered-oldest-first
  (let ((j (run-job (make-pipeline (list (lines-job-cmd "a"))) :background t)))
    (fg j)
    (let ((events (job-events j)))
      (is (search "started" (first events)))
      (is (search "done" (car (last events)))))))

;;; ===========================================================================
;;; Independence of concurrent jobs
;;; ===========================================================================

(test concurrent-jobs-are-independent
  (let ((a (run-job (make-pipeline (list (external "sh" "-c" "sleep 0.2; printf 'A\\n'")))
                    :background t))
        (b (run-job (make-pipeline (list (lines-job-cmd "B"))) :background t)))
    ;; b finishes quickly and independently while a is still running
    (is (equal '("B") (fg b)))
    (is (equal '("A") (fg a)))
    (is (/= (job-id a) (job-id b)))))

;;; ===========================================================================
;;; Edge cases
;;; ===========================================================================

(test stop-and-continue-repeatedly
  (let ((j (run-job (make-pipeline
                     (list (external "sh" "-c" "sleep 0.3; printf 'p\\nq\\n'")))
                    :background t)))
    (sleep 0.05)
    (stop-job j) (is (eq :stopped (job-state j)))
    (continue-job j) (is (eq :running (job-state j)))
    (stop-job j) (is (eq :stopped (job-state j)))
    (continue-job j)
    (is (equal '("p" "q") (fg j)))))

(test wait-job-timeout-returns-nil
  (let ((j (run-job (make-pipeline (list (external "sh" "-c" "sleep 1; printf 'x\\n'")))
                    :background t)))
    (multiple-value-bind (out ok) (wait-job j :timeout 0.1)
      (is (null out))
      (is (null ok)))                                  ; not done yet
    (kill-job j)))

(test debug-job-on-non-parked-errors
  (let ((j (run-job (make-pipeline (list (lines-job-cmd "a"))) :background t)))
    (fg j)
    (is (null (job-restarts j)))                       ; nothing parked
    (signals error (debug-job j))))

(test resume-job-when-not-parked-is-a-noop
  (let ((j (run-job (make-pipeline (list (lines-job-cmd "a"))) :background t)))
    (is (eq j (resume-job j)))                         ; harmless
    (is (equal '("a") (fg j)))))

(test kill-job-is-idempotent
  (let ((j (run-job (make-pipeline (list (lines-job-cmd "a"))) :background t)))
    (fg j)
    (finishes (kill-job j))
    (finishes (kill-job j))
    (is (eq :done (job-state j)))))

(test parked-job-buffers-what-preceded-the-error
  "While a job is parked, the output produced before the error is already
buffered (upstream frozen by backpressure)."
  (let ((j (run-job (make-pipeline (list (kv-source "a=1\\nb=2\\nBAD\\nc=3\\n")))
                    :on-parse-error :error :background t)))
    (is-true (poll-until (lambda () (eq :parked (job-state j)))))
    ;; a=1 and b=2 were emitted and drained into the buffer before BAD parked us
    (is-true (poll-until (lambda () (= 2 (length (job-output-list j))))))
    (is (equal (list (cons "a" "1") (cons "b" "2")) (job-output-list j)))
    (kill-job j)))

;;; ===========================================================================
;;; Bounded output: the ring buffer (SPEC §6) — `yes &` must not OOM
;;; ===========================================================================

(test ring-buffer-under-capacity-keeps-all
  (let ((rb (make-ring-buffer 5)))
    (dotimes (i 3) (ring-push rb i))
    (is (equal '(0 1 2) (ring-list rb)))
    (is (= 3 (ring-buffer-count rb)))
    (is (= 0 (ring-buffer-dropped rb)))))

(test ring-buffer-drops-oldest-when-full
  (let ((rb (make-ring-buffer 3)))
    (dotimes (i 7) (ring-push rb i))
    (is (equal '(4 5 6) (ring-list rb)))              ; only the last 3 survive
    (is (= 3 (ring-buffer-count rb)))
    (is (= 4 (ring-buffer-dropped rb)))))

(test ring-buffer-capacity-one
  (let ((rb (make-ring-buffer 1)))
    (ring-push rb :a)
    (ring-push rb :b)
    (is (equal '(:b) (ring-list rb)))
    (is (= 1 (ring-buffer-dropped rb)))))

(test finite-job-keeps-full-output-and-drops-nothing
  (let ((j (run-job (make-pipeline (list (lines-job-cmd "a" "b" "c"))) :background t)))
    (is (equal '("a" "b" "c") (fg j)))
    (is (= 0 (job-output-dropped j)))))

(test background-job-output-is-bounded
  "An unbounded producer (`yes`) keeps only the last BUFFER-CAPACITY objects and
counts the rest as dropped — bounded memory, no OOM."
  (let ((j (run-job (make-pipeline (list (external "yes")))
                    :background t :buffer-capacity 16)))
    (sleep 0.2)
    (kill-job j)
    (is (<= (length (job-output-list j)) 16))
    (is (> (job-output-dropped j) 0))
    (is (every (lambda (x) (equal x "y")) (job-output-list j)))))

;;; ===========================================================================
;;; Controlling terminal / real job control
;;; ===========================================================================

(test terminal-job-control-inactive-by-default
  (let ((*terminal-fd* nil) (*shell-pgid* nil))
    (is-false (terminal-job-control-active-p))))

(test enable-terminal-job-control-is-a-noop-on-a-non-tty
  "A pipe fd is not a terminal, so enabling job control on it does nothing and
leaves the shell in non-interactive mode."
  (multiple-value-bind (r w) (make-pipe)
    (unwind-protect
         (let ((*terminal-fd* nil) (*shell-pgid* nil))
           (is (null (enable-terminal-job-control r)))
           (is (null *terminal-fd*))
           (is-false (terminal-job-control-active-p)))
      (c-close r) (c-close w))))

(test terminal-handoff-is-safe-when-inactive
  "give-terminal-to-job / reclaim-terminal are no-ops (never error) with no tty."
  (let ((*terminal-fd* nil) (*shell-pgid* nil)
        (j (run-job (make-pipeline (list (external "sh" "-c" "printf 'x\\n'")))
                    :background t)))
    (finishes (reclaim-terminal))
    (finishes (give-terminal-to-job j))
    (is (equal '("x") (fg j)))))                     ; fg still works with no tty

(test job-stopped-predicate-false-for-a-running-job
  (let ((j (run-job (make-pipeline (list (external "sleep" "5"))) :background t)))
    (unwind-protect (is-false (consh::%job-stopped-p j))
      (kill-job j))))
