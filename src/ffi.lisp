;;;; ffi.lisp — raw CFFI syscall bindings + high-level SPAWN
;;;;
;;;; Everything foreign lives in this file (CLAUDE.md: "Group all raw syscall
;;;; bindings in src/ffi.lisp").  We target SBCL on Linux and macOS.  The wait
;;;; status encoding and the fcntl / posix_spawn flag constants used here are
;;;; identical across the two platforms; where the opaque posix_spawn control
;;;; blocks differ in size (macOS: a single pointer; glibc: an in-line struct)
;;;; we simply over-allocate a zeroed buffer big enough for either.

(in-package #:consh.ffi)

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; sb-posix gives us an ABI-correct fcntl.  fcntl is variadic, and on Darwin
  ;; arm64 variadic arguments are passed on the stack while CFFI's foreign-funcall
  ;; would place them in registers — so a naive CFFI fcntl silently corrupts the
  ;; FD_CLOEXEC argument there.  sb-posix wraps it correctly on every platform.
  (require :sb-posix))

;;; ---------------------------------------------------------------------------
;;; Conditions
;;; ---------------------------------------------------------------------------

(define-condition ffi-error (error)
  ((syscall :initarg :syscall :reader ffi-error-syscall)
   (errno   :initarg :errno   :reader ffi-error-errno :initform nil)
   (message :initarg :message :reader ffi-error-message :initform nil))
  (:report (lambda (c s)
             (format s "~A failed~@[ (errno ~D~@[ ~A~])~]~@[: ~A~]"
                     (ffi-error-syscall c)
                     (ffi-error-errno c)
                     (and (ffi-error-errno c) (errno-name (ffi-error-errno c)))
                     (ffi-error-message c))))
  (:documentation "A raw syscall returned an error indication."))

(define-condition spawn-error (ffi-error)
  ((program :initarg :program :reader spawn-error-program))
  (:report (lambda (c s)
             (format s "failed to spawn ~S~@[ (errno ~D~@[ ~A~])~]"
                     (spawn-error-program c)
                     (ffi-error-errno c)
                     (and (ffi-error-errno c) (errno-name (ffi-error-errno c)))))))

;;; ---------------------------------------------------------------------------
;;; errno
;;; ---------------------------------------------------------------------------
;;;
;;; errno is a thread-local expanding to (*__error()) on macOS and
;;; (*__errno_location()) on glibc.  Resolve whichever accessor exists and read
;;; through it.  Read immediately after the failing call, before any other
;;; foreign call can clobber it.

;;; Foreign-symbol pointers are re-resolved at image startup: a pointer cached at
;;; load time is stale after save-lisp-and-die (ASLR moves libc), so a dumped
;;; consh executable must recompute them on restore.  See %resolve-ffi-symbols
;;; and the *init-hooks* registration at the end of this file.

(defvar *errno-location-fn* nil
  "Pointer to the C function returning &errno for the calling thread.")

(defun get-errno ()
  "Current thread's errno."
  (if *errno-location-fn*
      (cffi:mem-ref (cffi:foreign-funcall-pointer *errno-location-fn* () :pointer)
                    :int)
      0))

;;; A handful of errno constants (values agree across Linux and macOS unless a
;;; platform branch is shown).
(defconstant +eintr+  4)
(defconstant +echild+ 10)
(defconstant +eperm+  1)
(defconstant +eagain+ (if (member :darwin *features*) 35 11))

;; Bounded retry for transient posix_spawn failures (EAGAIN / spurious EPERM).
(defparameter +spawn-max-attempts+ 8)
(defparameter +spawn-retry-seconds+ 0.025)

(defun errno-name (errno)
  "Best-effort symbolic name for ERRNO via strerror."
  (when (and errno (/= errno 0))
    (let ((p (cffi:foreign-funcall "strerror" :int errno :pointer)))
      (unless (cffi:null-pointer-p p)
        (cffi:foreign-string-to-lisp p)))))

;;; ---------------------------------------------------------------------------
;;; Signals (numbers common to Linux and macOS)
;;; ---------------------------------------------------------------------------

(defconstant +sigint+  2)
(defconstant +sigkill+ 9)
(defconstant +sigterm+ 15)
(defconstant +sigcont+ (if (member :darwin *features*) 19 18))
(defconstant +sigstop+ (if (member :darwin *features*) 17 19))
(defconstant +sigtstp+ (if (member :darwin *features*) 18 20))
(defconstant +sigchld+ (if (member :darwin *features*) 20 17))
(defconstant +sigttin+ 21)              ; same on Linux and macOS
(defconstant +sigttou+ 22)

(defmacro with-signal-ignored ((signum) &body body)
  "Run BODY with SIGNUM set to SIG_IGN, restoring the previous disposition after.
Used to shield the shell from the SIGTTOU it would receive when calling
tcsetpgrp from a background process group."
  (let ((sig (gensym "SIG")) (old (gensym "OLD")))
    `(let* ((,sig ,signum)
            ;; signal(2): SIG_IGN is the pointer value 1; returns the old handler.
            (,old (cffi:foreign-funcall "signal" :int ,sig
                                        :pointer (cffi:make-pointer 1) :pointer)))
       (unwind-protect (progn ,@body)
         (cffi:foreign-funcall "signal" :int ,sig :pointer ,old :pointer)))))

;;; ---------------------------------------------------------------------------
;;; Small helpers
;;; ---------------------------------------------------------------------------

(defmacro with-errno ((result-var errno-var) call &body body)
  "Bind RESULT-VAR to CALL and ERRNO-VAR to the errno captured immediately
after, then run BODY."
  `(let* ((,result-var ,call)
          (,errno-var (get-errno)))
     ,@body))

(defun call-retrying-eintr (thunk)
  "Call THUNK; if it returns (values -1 EINTR) retry.  THUNK returns
(values result errno)."
  (loop
    (multiple-value-bind (res errno) (funcall thunk)
      (unless (and (eql res -1) (eql errno +eintr+))
        (return (values res errno))))))

;;; ---------------------------------------------------------------------------
;;; pipe(2), close(2), fcntl(2) CLOEXEC
;;; ---------------------------------------------------------------------------

(defconstant +f-getfd+   1)
(defconstant +f-setfd+   2)
(defconstant +fd-cloexec+ 1)

(defun make-pipe (&key (cloexec t))
  "Create a pipe.  Returns (values read-fd write-fd).  Both ends get
FD_CLOEXEC set unless :CLOEXEC nil, so they never leak across an exec they
were not explicitly dup2'd into."
  (cffi:with-foreign-object (fds :int 2)
    (with-errno (rc errno) (cffi:foreign-funcall "pipe" :pointer fds :int)
      (when (/= rc 0)
        (error 'ffi-error :syscall "pipe" :errno errno)))
    (let ((r (cffi:mem-aref fds :int 0))
          (w (cffi:mem-aref fds :int 1)))
      (when cloexec
        (set-cloexec r t)
        (set-cloexec w t))
      (values r w))))

(defun c-close (fd)
  "close(2).  Returns T on success.  EINTR is retried."
  (multiple-value-bind (rc errno)
      (call-retrying-eintr
       (lambda () (with-errno (rc e) (cffi:foreign-funcall "close" :int fd :int)
                    (values rc e))))
    (if (= rc 0)
        t
        (error 'ffi-error :syscall "close" :errno errno))))

(defun set-cloexec (fd on)
  "Set or clear FD_CLOEXEC on FD."
  (let* ((flags (sb-posix:fcntl fd +f-getfd+))
         (new (if on
                  (logior flags +fd-cloexec+)
                  (logandc2 flags +fd-cloexec+))))
    (sb-posix:fcntl fd +f-setfd+ new)
    on))

(defun cloexec-p (fd)
  "True if FD has FD_CLOEXEC set."
  (logtest (sb-posix:fcntl fd +f-getfd+) +fd-cloexec+))

;;; ---------------------------------------------------------------------------
;;; waitpid(2)
;;; ---------------------------------------------------------------------------

(defconstant +wnohang+   1)
(defconstant +wuntraced+ 2)

(defun c-waitpid (pid options)
  "Thin waitpid(2).  Returns (values reaped-pid raw-status errno).
reaped-pid is 0 when WNOHANG found nothing ready, -1 on error (errno then
meaningful, e.g. ECHILD)."
  (cffi:with-foreign-object (status :int)
    (multiple-value-bind (rc errno)
        (call-retrying-eintr
         (lambda ()
           (with-errno (rc e)
               (cffi:foreign-funcall "waitpid"
                                     :int pid :pointer status :int options :int)
             (values rc e))))
      (values rc (cffi:mem-ref status :int) errno))))

(defun decode-wait-status (raw)
  "Decode a raw wait status into (values kind code), where KIND is one of
:EXITED :SIGNALED :STOPPED :CONTINUED and CODE is the exit code or signal
number.  Encoding is the classic wait(2) layout shared by Linux and macOS."
  (let ((low (logand raw #x7f)))
    (cond
      ;; WIFEXITED: low 7 bits zero
      ((= low 0)
       (values :exited (logand (ash raw -8) #xff)))
      ;; WIFSTOPPED: low byte == 0x7f
      ((= (logand raw #xff) #x7f)
       (values :stopped (logand (ash raw -8) #xff)))
      ;; WIFCONTINUED: status == 0xffff
      ((= raw #xffff)
       (values :continued +sigcont+))
      ;; else WIFSIGNALED
      (t
       (values :signaled low)))))

;;; ---------------------------------------------------------------------------
;;; kill(2), killpg(2), setpgid(2), getpgid(2), tcsetpgrp(3)
;;; ---------------------------------------------------------------------------

(defun c-kill (pid signal)
  "kill(2).  Returns T on success."
  (with-errno (rc errno) (cffi:foreign-funcall "kill" :int pid :int signal :int)
    (if (= rc 0) t
        (error 'ffi-error :syscall "kill" :errno errno))))

(defun c-killpg (pgid signal)
  "killpg(2).  PGID is the (positive) process-group id.  Returns T."
  (with-errno (rc errno) (cffi:foreign-funcall "killpg" :int pgid :int signal :int)
    (if (= rc 0) t
        (error 'ffi-error :syscall "killpg" :errno errno))))

(defun c-setpgid (pid pgid)
  "setpgid(2)."
  (with-errno (rc errno) (cffi:foreign-funcall "setpgid" :int pid :int pgid :int)
    (if (= rc 0) t
        (error 'ffi-error :syscall "setpgid" :errno errno))))

(defun c-getpgid (pid)
  "getpgid(2).  Returns the process-group id of PID."
  (with-errno (rc errno) (cffi:foreign-funcall "getpgid" :int pid :int)
    (if (>= rc 0) rc
        (error 'ffi-error :syscall "getpgid" :errno errno))))

(defun c-tcsetpgrp (fd pgid)
  "tcsetpgrp(3): make PGID the foreground group of terminal FD."
  (with-errno (rc errno) (cffi:foreign-funcall "tcsetpgrp" :int fd :int pgid :int)
    (if (= rc 0) t
        (error 'ffi-error :syscall "tcsetpgrp" :errno errno))))

(defun c-tcgetpgrp (fd)
  "tcgetpgrp(3): the foreground process-group id of terminal FD, or NIL on error
(e.g. FD is not a terminal)."
  (let ((rc (cffi:foreign-funcall "tcgetpgrp" :int fd :int)))
    (if (>= rc 0) rc nil)))

(defun c-isatty (fd)
  "isatty(3): true when FD refers to a terminal."
  (= 1 (cffi:foreign-funcall "isatty" :int fd :int)))

;;; ---------------------------------------------------------------------------
;;; posix_spawn(3) + file_actions + attributes
;;; ---------------------------------------------------------------------------
;;;
;;; The opaque control blocks: on macOS both are typedef'd to a single pointer;
;;; on glibc they are in-line structs (~80 and ~336 bytes).  A zeroed 1 KiB
;;; buffer is a safe superset for either interpretation.

(defconstant +spawn-ctrl-block-size+ 1024)

(defconstant +posix-spawn-setpgroup+ #x02
  "POSIX_SPAWN_SETPGROUP — same value on Linux and macOS.")

(defvar *addchdir-fn* nil
  "Pointer to the available addchdir file-action, or NIL if the libc predates
it.  We keep the pointer (not the name) because it is called through
foreign-funcall-pointer at a computed address.  Re-resolved on image startup.")

(defun posix-spawn-available-addchdir-p ()
  "True if this libc can perform a per-spawn chdir file action (glibc >= 2.29,
macOS >= 10.15).  consh relies on this to honor *current-directory* without a
process-wide chdir."
  (and *addchdir-fn* t))

;;; environ, for env inheritance.  Re-resolved on image startup.
(defvar *environ-symbol* nil)

(defun %resolve-ffi-symbols ()
  "Resolve (or re-resolve) the cached foreign-symbol pointers.  Run at load time
and again on image restore so a dumped consh executable does not dereference
addresses left over from the build-time process."
  (setf *errno-location-fn*
        (or (cffi:foreign-symbol-pointer "__error")            ; macOS / BSD
            (cffi:foreign-symbol-pointer "__errno_location"))  ; glibc
        *addchdir-fn*
        (or (cffi:foreign-symbol-pointer "posix_spawn_file_actions_addchdir_np")
            (cffi:foreign-symbol-pointer "posix_spawn_file_actions_addchdir"))
        *environ-symbol*
        (cffi:foreign-symbol-pointer "environ"))
  (values))

(%resolve-ffi-symbols)                                   ; at load
(pushnew '%resolve-ffi-symbols sb-ext:*init-hooks*)      ; and on image startup

(defun current-environ-pointer ()
  "The live char** environ of this process, for passing straight through to
posix_spawn when the caller wants inherited environment."
  (if *environ-symbol*
      (cffi:mem-ref *environ-symbol* :pointer)
      (cffi:null-pointer)))

(defmacro with-foreign-string-vector ((var lisp-strings) &body body)
  "Bind VAR to a freshly allocated NULL-terminated char* [] built from
LISP-STRINGS (a list of strings), freeing every string and the vector on exit."
  (let ((strings (gensym "STRINGS")) (n (gensym "N")) (i (gensym "I"))
        (s (gensym "S")))
    `(let* ((,strings (coerce ,lisp-strings 'list))
            (,n (length ,strings))
            (,var (cffi:foreign-alloc :pointer :count (1+ ,n))))
       (unwind-protect
            (progn
              (loop for ,i from 0
                    for ,s in ,strings
                    do (setf (cffi:mem-aref ,var :pointer ,i)
                             (cffi:foreign-string-alloc ,s)))
              (setf (cffi:mem-aref ,var :pointer ,n) (cffi:null-pointer))
              ,@body)
         (dotimes (,i ,n)
           (cffi:foreign-string-free (cffi:mem-aref ,var :pointer ,i)))
         (cffi:foreign-free ,var)))))

(defun %build-envp-strings (environment)
  "Normalize ENVIRONMENT into a list of \"KEY=VALUE\" strings.  Accepts either
that form already, or an alist of (KEY . VALUE) / (KEY VALUE)."
  (mapcar (lambda (e)
            (cond ((stringp e) e)
                  ((consp e)
                   (format nil "~A=~A"
                           (car e)
                           (if (consp (cdr e)) (cadr e) (cdr e))))
                  (t (princ-to-string e))))
          environment))

(defun spawn (program arguments
              &key directory environment (search t) pgid
                   file-actions)
  "Spawn PROGRAM (a namestring) with ARGUMENTS (a list of strings, not
including argv[0]).  Returns the child pid.

  :DIRECTORY    when non-NIL, the child chdir's here (via a spawn file action)
                before exec — this is how consh honors *current-directory*
                with no process-wide chdir.  Signals if the libc lacks
                addchdir support.
  :ENVIRONMENT  NIL inherits this process's environment; otherwise a list of
                \"K=V\" strings or an alist becomes the child's entire env.
  :SEARCH       T (default) resolves PROGRAM via PATH when it contains no
                slash (posix_spawnp); NIL uses PROGRAM as a literal path.
  :PGID         when supplied, the child joins process group PGID (0 = start a
                new group led by the child).
  :FILE-ACTIONS a list of extra actions applied in order, each one of:
                  (:dup2 oldfd newfd) (:close fd) (:open fd path flags mode)
                These run in the child before exec.  DIRECTORY's chdir, if any,
                is applied first."
  (check-type program string)
  (let ((argv-list (cons program (mapcar #'princ-to-string arguments)))
        (fa-buf (cffi:null-pointer))
        (attr-buf (cffi:null-pointer))
        (fa-inited nil)
        (attr-inited nil)
        (want-fa (or directory file-actions))
        (want-attr pgid))
    (when (and directory (not (posix-spawn-available-addchdir-p)))
      (error 'spawn-error :program program :syscall "posix_spawn"
             :message "libc lacks posix_spawn_file_actions_addchdir(_np); cannot honor :directory"))
    (unwind-protect
         (progn
           ;; --- file actions ---
           (when want-fa
             (setf fa-buf (cffi:foreign-alloc :char :count +spawn-ctrl-block-size+))
             (dotimes (i +spawn-ctrl-block-size+) (setf (cffi:mem-aref fa-buf :char i) 0))
             (let ((rc (cffi:foreign-funcall "posix_spawn_file_actions_init"
                                             :pointer fa-buf :int)))
               (unless (= rc 0)
                 (error 'spawn-error :program program
                        :syscall "posix_spawn_file_actions_init" :errno rc)))
             (setf fa-inited t)
             (when directory
               (let ((rc (cffi:foreign-funcall-pointer
                          *addchdir-fn* ()
                          :pointer fa-buf
                          :string (namestring directory)
                          :int)))
                 (unless (= rc 0)
                   (error 'spawn-error :program program
                          :syscall "posix_spawn_file_actions_addchdir" :errno rc))))
             (dolist (act file-actions)
               (%apply-file-action fa-buf act program)))
           ;; --- attributes (process group) ---
           (when want-attr
             (setf attr-buf (cffi:foreign-alloc :char :count +spawn-ctrl-block-size+))
             (dotimes (i +spawn-ctrl-block-size+) (setf (cffi:mem-aref attr-buf :char i) 0))
             (let ((rc (cffi:foreign-funcall "posix_spawnattr_init"
                                             :pointer attr-buf :int)))
               (unless (= rc 0)
                 (error 'spawn-error :program program
                        :syscall "posix_spawnattr_init" :errno rc)))
             (setf attr-inited t)
             (let ((rc (cffi:foreign-funcall "posix_spawnattr_setflags"
                                             :pointer attr-buf
                                             :short +posix-spawn-setpgroup+ :int)))
               (unless (= rc 0)
                 (error 'spawn-error :program program
                        :syscall "posix_spawnattr_setflags" :errno rc)))
             (let ((rc (cffi:foreign-funcall "posix_spawnattr_setpgroup"
                                             :pointer attr-buf :int pgid :int)))
               (unless (= rc 0)
                 (error 'spawn-error :program program
                        :syscall "posix_spawnattr_setpgroup" :errno rc))))
           ;; --- spawn ---
           (cffi:with-foreign-object (pid-out :int)
             (with-foreign-string-vector (argv argv-list)
               (let* ((envp (if environment
                                nil            ; filled below via nested macro
                                (current-environ-pointer)))
                      (fn-name (if search "posix_spawnp" "posix_spawn"))
                      (fn-ptr (cffi:foreign-symbol-pointer fn-name)))
                 (flet ((do-spawn (envp-ptr)
                          ;; Called through a computed address: FN-PTR is chosen
                          ;; at runtime, so foreign-funcall (literal names only)
                          ;; won't do — use foreign-funcall-pointer.
                          (cffi:foreign-funcall-pointer
                           fn-ptr ()
                           :pointer pid-out
                           :string program
                           :pointer (if want-fa fa-buf (cffi:null-pointer))
                           :pointer (if want-attr attr-buf (cffi:null-pointer))
                           :pointer argv
                           :pointer envp-ptr
                           :int)))
                   (flet ((attempt ()
                            (if environment
                                (with-foreign-string-vector
                                    (custom-envp (%build-envp-strings environment))
                                  (do-spawn custom-envp))
                                (do-spawn envp))))
                     ;; posix_spawn can fail transiently under resource/scheduler
                     ;; pressure — EAGAIN (fork limit) and, on some sandboxed
                     ;; hosts (GitHub's macOS runners), a spurious EPERM.  Retry a
                     ;; few times with a short backoff; a genuine error still
                     ;; surfaces after the last attempt.
                     (let ((rc (loop for tries from 1
                                     for result = (attempt)
                                     when (or (= result 0)
                                              (not (or (= result +eagain+) (= result +eperm+)))
                                              (>= tries +spawn-max-attempts+))
                                       return result
                                     do (sleep +spawn-retry-seconds+))))
                       (unless (= rc 0)
                         (error 'spawn-error :program program :syscall fn-name :errno rc))
                       (cffi:mem-ref pid-out :int))))))))
      ;; cleanup
      (when fa-inited
        (cffi:foreign-funcall "posix_spawn_file_actions_destroy" :pointer fa-buf :int))
      (unless (cffi:null-pointer-p fa-buf) (cffi:foreign-free fa-buf))
      (when attr-inited
        (cffi:foreign-funcall "posix_spawnattr_destroy" :pointer attr-buf :int))
      (unless (cffi:null-pointer-p attr-buf) (cffi:foreign-free attr-buf)))))

(defun %apply-file-action (fa-buf act program)
  "Apply one extra file action ACT to the file-actions block FA-BUF."
  (ecase (car act)
    (:dup2
     (destructuring-bind (oldfd newfd) (cdr act)
       (let ((rc (cffi:foreign-funcall "posix_spawn_file_actions_adddup2"
                                       :pointer fa-buf :int oldfd :int newfd :int)))
         (unless (= rc 0)
           (error 'spawn-error :program program
                  :syscall "posix_spawn_file_actions_adddup2" :errno rc)))))
    (:close
     (let ((rc (cffi:foreign-funcall "posix_spawn_file_actions_addclose"
                                     :pointer fa-buf :int (second act) :int)))
       (unless (= rc 0)
         (error 'spawn-error :program program
                :syscall "posix_spawn_file_actions_addclose" :errno rc))))
    (:open
     (destructuring-bind (fd path flags mode) (cdr act)
       (let ((rc (cffi:foreign-funcall "posix_spawn_file_actions_addopen"
                                       :pointer fa-buf :int fd
                                       :string (namestring path)
                                       :int flags :unsigned-int mode :int)))
         (unless (= rc 0)
           (error 'spawn-error :program program
                  :syscall "posix_spawn_file_actions_addopen" :errno rc)))))))

;;; ---------------------------------------------------------------------------
;;; stat(2) and getpwuid(3) — the enrichment primitives (Phase 3)
;;; ---------------------------------------------------------------------------
;;;
;;; `struct stat` differs in layout across platforms and `stat` itself is often
;;; a versioned/inline symbol (glibc __xstat, macOS $INODE64 variants), so we go
;;; through sb-posix which encodes the right ABI per platform.  getpwuid is a
;;; plain (non-variadic) call and `struct passwd` begins with `char *pw_name` on
;;; both Linux and macOS, so we read that first field directly via CFFI.

(defun stat-fields (path)
  "Stat PATH, returning (values size mtime-seconds mode uid gid).  Signals
FFI-ERROR if the file cannot be stat'd (e.g. it vanished)."
  (handler-case
      (let ((st (sb-posix:stat (namestring path))))
        (values (sb-posix:stat-size st)
                (floor (sb-posix:stat-mtime st))
                (sb-posix:stat-mode st)
                (sb-posix:stat-uid st)
                (sb-posix:stat-gid st)))
    (sb-posix:syscall-error (e)
      (error 'ffi-error :syscall "stat"
             :errno (sb-posix:syscall-errno e)
             :message (namestring path)))))

(defun uid-username (uid)
  "The login name for UID via getpwuid(3), or NIL if unknown.  Reads pw_name,
the first member of struct passwd on both Linux and macOS."
  (let ((pw (cffi:foreign-funcall "getpwuid" :uint32 uid :pointer)))
    (unless (cffi:null-pointer-p pw)
      (let ((name-ptr (cffi:mem-ref pw :pointer 0)))   ; offset 0 == pw_name
        (unless (cffi:null-pointer-p name-ptr)
          (cffi:foreign-string-to-lisp name-ptr))))))
