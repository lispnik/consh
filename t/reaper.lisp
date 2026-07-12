;;;; t/reaper.lisp — Phase 1 acceptance: spawn + reaper + status + no zombies.

(in-package #:consh/test)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :sb-posix))

(def-suite reaper :in consh :description "Spawning and the waitpid reaper.")
(in-suite reaper)

;;; Ensure a fresh reaper around each test so no-zombie assertions are not
;;; disturbed by leftover state.
(def-fixture with-reaper ()
  (stop-reaper)
  (ensure-reaper)
  (unwind-protect (&body)
    (stop-reaper)))

(defun make-temp-dir ()
  "Create and return a fresh temporary directory pathname."
  (let ((path (format nil "/tmp/consh-test-~D-~D/"
                      (sb-posix:getpid) (get-internal-real-time))))
    (ensure-directories-exist path)
    (truename path)))

;;; -------------------------------------------------------------------------
;;; true / false: statuses arrive correctly via the reaper
;;; -------------------------------------------------------------------------

(test spawn-true-succeeds
  (with-fixture with-reaper ()
    (let ((p (launch "true")))
      (is-true (wait-process p :timeout 5))
      (is (eq :exited (process-status p)))
      (is (= 0 (process-exit-code p)))
      (is-true (process-success-p p)))))

(test spawn-false-fails
  (with-fixture with-reaper ()
    (let ((p (launch "false")))
      (is-true (wait-process p :timeout 5))
      (is (eq :exited (process-status p)))
      (is (= 1 (process-exit-code p)))
      (is-false (process-success-p p)))))

(test signaled-status
  "A process killed by a signal reports :signaled with that signal."
  (with-fixture with-reaper ()
    (let ((p (launch "sleep" '("30"))))
      ;; give it a moment to actually start, then kill it
      (sleep 0.1)
      (kill-process p)                  ; SIGKILL
      (is-true (wait-process p :timeout 5))
      (is (eq :signaled (process-status p)))
      (is (= +sigkill+ (process-term-signal p))))))

;;; -------------------------------------------------------------------------
;;; 100 concurrent sleeps: all reaped, no zombies
;;; -------------------------------------------------------------------------

(test hundred-concurrent-sleeps
  (with-fixture with-reaper ()
    (let ((procs (loop repeat 100 collect (launch "sleep" '("0.1")))))
      (is (= 100 (length procs)))
      ;; Everyone should finish well within a few seconds.
      (dolist (p procs)
        (is-true (wait-process p :timeout 10)))
      (is (every #'process-success-p procs))
      ;; All tracked processes have been reaped and forgotten.
      (is (= 0 (live-process-count)))
      ;; Stop the reaper so it isn't racing our probe, then assert the kernel
      ;; reports no children at all: waitpid(-1) -> ECHILD.
      (stop-reaper)
      (multiple-value-bind (pid raw errno)
          (c-waitpid -1 (logior +wnohang+ +wuntraced+))
        (declare (ignore raw))
        (is (= -1 pid))
        (is (= +echild+ errno))))))

;;; -------------------------------------------------------------------------
;;; relative-path spawn honors *current-directory*, no process-wide chdir
;;; -------------------------------------------------------------------------

(test relative-path-honors-current-directory
  (with-fixture with-reaper ()
    (let* ((dir (make-temp-dir))
           (script (merge-pathnames "probe.sh" dir))
           (marker (merge-pathnames "cwd_marker" dir))
           (cwd-before (sb-posix:getcwd)))
      (with-open-file (s script :direction :output :if-exists :supersede)
        (format s "#!/bin/sh~%/bin/pwd > cwd_marker~%"))
      (sb-posix:chmod (namestring script) #o755)
      ;; Spawn by a RELATIVE program path, with the shell cwd set to DIR.
      (let ((p (launch "./probe.sh" '() :directory dir)))
        (is-true (wait-process p :timeout 5))
        (is (eq :exited (process-status p)))
        (is (= 0 (process-exit-code p))))
      ;; The relative write landed inside DIR => the child's cwd was DIR.
      (is-true (probe-file marker))
      (let ((reported (with-open-file (s marker) (read-line s nil ""))))
        (is (equal (truename reported) (truename dir))))
      ;; And the shell process itself never chdir'd.
      (is (equal cwd-before (sb-posix:getcwd))))))

;;; -------------------------------------------------------------------------
;;; Exit codes and argument passing
;;; -------------------------------------------------------------------------

(test arbitrary-exit-code
  "A non-0/1 exit code is reported faithfully across the full byte range."
  (with-fixture with-reaper ()
    (dolist (code '(2 42 127 255))
      (let ((p (launch "/bin/sh" (list "-c" (format nil "exit ~D" code))
                       :search nil)))
        (is-true (wait-process p :timeout 5))
        (is (eq :exited (process-status p)))
        (is (= code (process-exit-code p)))))))

(test arguments-are-passed
  "ARGUMENTS reach the child in order: sh sees the right count of positional
args after argv0."
  (with-fixture with-reaper ()
    ;; argv = sh -c 'exit $#' argv0 a b c  =>  $# counts a,b,c = 3
    (let ((p (launch "/bin/sh" '("-c" "exit $#" "argv0" "a" "b" "c")
                     :search nil)))
      (is-true (wait-process p :timeout 5))
      (is (= 3 (process-exit-code p))))))

(test argument-values-reach-child
  "Argument *values* (not just their count) arrive intact."
  (with-fixture with-reaper ()
    ;; exits 0 only if $1 is exactly the string we passed
    (let ((p (launch "/bin/sh"
                     '("-c" "[ \"$1\" = consh-arg ] && exit 0 || exit 7"
                       "argv0" "consh-arg")
                     :search nil)))
      (is-true (wait-process p :timeout 5))
      (is (= 0 (process-exit-code p))))))

;;; -------------------------------------------------------------------------
;;; Environment: inherited by default, fully replaced when supplied
;;; -------------------------------------------------------------------------

(test environment-inherited-by-default
  "With no :environment, the child inherits this process's environment."
  (with-fixture with-reaper ()
    (sb-posix:setenv "CONSH_INHERIT" "5" 1)
    (unwind-protect
         (let ((p (launch "/bin/sh" '("-c" "exit ${CONSH_INHERIT}")
                          :search nil)))
           (is-true (wait-process p :timeout 5))
           (is (= 5 (process-exit-code p))))
      (sb-posix:unsetenv "CONSH_INHERIT"))))

(test environment-supplied-is-visible
  "A variable given via :environment is visible to the child."
  (with-fixture with-reaper ()
    (let ((p (launch "/bin/sh" '("-c" "exit ${MYV}")
                     :search nil :environment '("MYV=7"))))
      (is-true (wait-process p :timeout 5))
      (is (= 7 (process-exit-code p))))))

(test environment-supplied-replaces-inherited
  ":environment is the child's *entire* environment — inherited vars vanish."
  (with-fixture with-reaper ()
    (sb-posix:setenv "CONSH_INHERIT" "5" 1)
    (unwind-protect
         (let ((p (launch "/bin/sh" '("-c" "exit ${CONSH_INHERIT:-9}")
                          :search nil :environment '("OTHER=1"))))
           (is-true (wait-process p :timeout 5))
           (is (= 9 (process-exit-code p))))    ; not 5 => inherited env replaced
      (sb-posix:unsetenv "CONSH_INHERIT"))))

(test environment-alist-accepted
  ":environment also accepts an alist of (KEY . VALUE)."
  (with-fixture with-reaper ()
    (let ((p (launch "/bin/sh" '("-c" "exit ${AV}")
                     :search nil :environment '(("AV" . "3")))))
      (is-true (wait-process p :timeout 5))
      (is (= 3 (process-exit-code p))))))

;;; -------------------------------------------------------------------------
;;; PATH search vs. literal path; spawn failures are conditions
;;; -------------------------------------------------------------------------

(test absolute-path-no-search
  "An absolute program path runs with :search nil (no PATH lookup)."
  (with-fixture with-reaper ()
    (let ((p (launch "/usr/bin/true" '() :search nil)))
      (is-true (wait-process p :timeout 5))
      (is-true (process-success-p p)))))

(test nonexistent-absolute-path-signals
  "Spawning a nonexistent absolute path signals spawn-error, not a phantom
process."
  (with-fixture with-reaper ()
    (signals spawn-error
      (launch "/nonexistent/consh-not-a-real-binary" '() :search nil))))

(test nonexistent-command-on-path-signals
  "A command name not found on PATH signals spawn-error."
  (with-fixture with-reaper ()
    (signals spawn-error
      (launch "consh-definitely-not-a-command-zzzq"))))

;;; -------------------------------------------------------------------------
;;; File actions: redirect the child's stdout
;;; -------------------------------------------------------------------------

(test open-file-action-redirects-stdout
  "An :open file action points the child's stdout at a file."
  (with-fixture with-reaper ()
    (let* ((dir (make-temp-dir))
           (out (merge-pathnames "out.txt" dir)))
      (let ((p (launch "/bin/sh" '("-c" "echo consh-file-ok")
                       :search nil
                       :file-actions
                       (list (list :open 1 out
                                   (logior sb-posix:o-wronly
                                           sb-posix:o-creat
                                           sb-posix:o-trunc)
                                   #o644)))))
        (is-true (wait-process p :timeout 5))
        (is-true (process-success-p p)))
      (is (equal "consh-file-ok"
                 (with-open-file (s out) (read-line s nil "")))))))

(test dup2-file-action-through-pipe
  "A :dup2 file action wires the child's stdout to a pipe the parent reads —
bytes flow child -> pipe(2) -> parent."
  (with-fixture with-reaper ()
    (multiple-value-bind (r w) (make-pipe)   ; both CLOEXEC in the parent
      (let ((in (sb-sys:make-fd-stream r :input t :element-type 'character))
            (line nil))
        (unwind-protect
             (let ((p (launch "/bin/sh" '("-c" "echo consh-pipe-ok")
                              :search nil
                              ;; dup2 clears CLOEXEC on the child's fd 1, so it
                              ;; survives exec while the CLOEXEC original closes.
                              :file-actions (list (list :dup2 w 1)))))
               (c-close w)                 ; parent drops the write end
               (setf line (read-line in nil nil))
               (is-true (wait-process p :timeout 5))
               (is-true (process-success-p p))
               (is (equal "consh-pipe-ok" line)))
          (close in))))))                  ; closes r exactly once

;;; -------------------------------------------------------------------------
;;; Waiting: timeouts, idempotence
;;; -------------------------------------------------------------------------

(test wait-times-out-then-completes
  "wait-process returns NIL on timeout while the child is still running, and
the real result once it finishes."
  (with-fixture with-reaper ()
    (let ((p (launch "sleep" '("30"))))
      (is (null (wait-process p :timeout 0.1)))   ; still running
      (is (eq :running (process-status p)))
      (kill-process p)
      (is-true (wait-process p :timeout 5))
      (is (eq :signaled (process-status p))))))

(test wait-on-finished-is-idempotent
  "Waiting again on an already-terminated process returns immediately."
  (with-fixture with-reaper ()
    (let ((p (launch "true")))
      (is-true (wait-process p :timeout 5))
      (is-true (wait-process p :timeout 0))       ; no blocking, still terminated
      (is-true (process-exited-p p)))))

;;; -------------------------------------------------------------------------
;;; The stopped-process path through the reaper (WUNTRACED)
;;; -------------------------------------------------------------------------

(test stop-then-kill-tracks-status
  "SIGSTOP is observed as :stopped (child stays tracked); a subsequent SIGKILL
transitions it to :signaled and removes it from the registry."
  (with-fixture with-reaper ()
    (let ((p (launch "sleep" '("30"))))
      (sleep 0.15)                          ; let it actually start
      (is (eq :running (process-status p)))
      (signal-process p +sigstop+)
      (is (eq :stopped (process-status-changed p :running :timeout 5)))
      (is (= +sigstop+ (process-stop-signal p)))
      (is-true (process-live-p p))          ; stopped is still "live"
      (is-false (process-exited-p p))
      (signal-process p +sigkill+)
      (is (eq :signaled (process-status-changed p :stopped :timeout 5)))
      (is (= +sigkill+ (process-term-signal p)))
      (is (= 0 (live-process-count))))))    ; reaped and forgotten

;;; -------------------------------------------------------------------------
;;; Registry bookkeeping under concurrency
;;; -------------------------------------------------------------------------

(test registry-drains-to-zero
  "After a burst of short-lived processes are all waited on, the live count
returns to zero (no leaked process objects)."
  (with-fixture with-reaper ()
    (let ((procs (loop repeat 25 collect (launch "true"))))
      (mapc (lambda (p) (wait-process p :timeout 10)) procs)
      (is (every #'process-success-p procs))
      (is (= 0 (live-process-count))))))

(test print-object-shows-pid-and-status
  "A process prints readably with its pid and status (useful at the REPL)."
  (with-fixture with-reaper ()
    (let ((p (launch "true")))
      (wait-process p :timeout 5)
      (let ((s (princ-to-string p)))
        (is (search "pid" s))
        (is (search (princ-to-string (process-pid p)) s))))))
