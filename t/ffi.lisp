;;;; t/ffi.lisp — low-level FFI suite: pipes, cloexec, wait-status decoding.

(in-package #:consh/test)

(def-suite ffi :in consh :description "Raw syscall bindings.")
(in-suite ffi)

(test pipe-and-cloexec
  "make-pipe returns two usable fds with FD_CLOEXEC set by default."
  (multiple-value-bind (r w) (make-pipe)
    (unwind-protect
         (progn
           (is (integerp r))
           (is (integerp w))
           (is (/= r w))
           (is-true (cloexec-p r))
           (is-true (cloexec-p w))
           ;; clearing works
           (set-cloexec r nil)
           (is-false (cloexec-p r)))
      (c-close r)
      (c-close w))))

(test decode-exited
  "A normal exit decodes to :exited with the right code."
  (multiple-value-bind (kind code) (decode-wait-status (ash 42 8)) ; WEXITSTATUS 42
    (is (eq :exited kind))
    (is (= 42 code)))
  (multiple-value-bind (kind code) (decode-wait-status 0)
    (is (eq :exited kind))
    (is (= 0 code))))

(test decode-signaled
  "Death by signal decodes to :signaled with the signal number."
  (multiple-value-bind (kind code) (decode-wait-status +sigterm+) ; low bits = signal
    (is (eq :signaled kind))
    (is (= +sigterm+ code))))

(test decode-stopped
  "A stop decodes to :stopped with the stopping signal."
  (multiple-value-bind (kind code)
      (decode-wait-status (logior #x7f (ash +sigtstp+ 8)))
    (is (eq :stopped kind))
    (is (= +sigtstp+ code))))

(test addchdir-available
  "The libc provides an addchdir file action (required to honor
*current-directory* without a global chdir)."
  (is-true (posix-spawn-available-addchdir-p)))

(test pipe-transfers-data
  "The two pipe ends are actually connected: a byte written to the write end
comes back out the read end."
  (multiple-value-bind (r w) (make-pipe)
    (let ((out (sb-sys:make-fd-stream w :output t :element-type '(unsigned-byte 8)))
          (in  (sb-sys:make-fd-stream r :input  t :element-type '(unsigned-byte 8))))
      (unwind-protect
           (progn
             (write-byte 65 out)          ; #\A
             (write-byte 90 out)          ; #\Z
             (finish-output out)
             (is (= 65 (read-byte in)))
             (is (= 90 (read-byte in))))
        ;; Closing the streams closes the underlying fds exactly once.
        (close out)
        (close in)))))

(test make-pipe-without-cloexec
  ":cloexec nil leaves both ends inheritable across exec."
  (multiple-value-bind (r w) (make-pipe :cloexec nil)
    (unwind-protect
         (progn
           (is-false (cloexec-p r))
           (is-false (cloexec-p w)))
      (c-close r)
      (c-close w))))

(test set-cloexec-idempotent
  "Setting then clearing then setting FD_CLOEXEC leaves it in the asked-for
state each time."
  (multiple-value-bind (r w) (make-pipe :cloexec nil)
    (unwind-protect
         (progn
           (set-cloexec r t)   (is-true  (cloexec-p r))
           (set-cloexec r t)   (is-true  (cloexec-p r))   ; idempotent
           (set-cloexec r nil) (is-false (cloexec-p r))
           (set-cloexec r nil) (is-false (cloexec-p r)))
      (c-close r)
      (c-close w))))

(test close-bad-fd-signals
  "Closing a nonsense fd raises an ffi-error (EBADF)."
  (signals ffi-error (c-close 999999)))

(test errno-name-known-and-zero
  "errno-name maps a real errno to a string and 0 to NIL."
  (is (stringp (errno-name +echild+)))
  (is (null (errno-name 0))))

(test waitpid-echild-on-stranger
  "waitpid on a pid that is not our child returns -1 with errno ECHILD,
exercising the get-errno wiring."
  (multiple-value-bind (pid raw errno) (c-waitpid 999999 +wnohang+)
    (declare (ignore raw))
    (is (= -1 pid))
    (is (= +echild+ errno))))

(test decode-exited-full-range
  "Exit codes use the full low byte; 255 round-trips."
  (multiple-value-bind (kind code) (decode-wait-status (ash 255 8))
    (is (eq :exited kind))
    (is (= 255 code)))
  (multiple-value-bind (kind code) (decode-wait-status (ash 1 8))
    (is (eq :exited kind))
    (is (= 1 code))))

(test decode-signaled-coredump
  "A core-dumping death (0x80 bit set) still decodes to :signaled with the
correct signal, ignoring the core-dump flag."
  (let ((sigsegv 11))
    (multiple-value-bind (kind code) (decode-wait-status (logior sigsegv #x80))
      (is (eq :signaled kind))
      (is (= sigsegv code)))))

(test signal-numbers-distinct
  "The job-control signals resolve to distinct positive numbers on this platform."
  (let ((sigs (list +sigint+ +sigkill+ +sigterm+ +sigcont+ +sigstop+ +sigtstp+)))
    (is (every #'plusp sigs))
    (is (= (length sigs) (length (remove-duplicates sigs))))))
