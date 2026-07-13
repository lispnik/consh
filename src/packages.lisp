;;;; packages.lisp — package definitions for consh

(defpackage #:consh.ffi
  (:use #:cl)
  (:documentation
   "Raw CFFI syscall bindings and the high-level SPAWN entry point.
    All raw foreign definitions live here; nothing above this layer
    calls a foreign function directly.")
  (:export
   ;; conditions
   #:ffi-error #:ffi-error-syscall #:ffi-error-errno #:ffi-error-message
   #:spawn-error #:spawn-error-program
   ;; errno / status helpers
   #:get-errno #:errno-name #:+echild+ #:+eintr+
   ;; pipe / fds
   #:make-pipe #:c-close #:set-cloexec #:cloexec-p
   ;; spawn
   #:spawn #:posix-spawn-available-addchdir-p
   ;; enrichment primitives (Phase 3)
   #:stat-fields #:uid-username
   ;; wait
   #:c-waitpid #:decode-wait-status
   #:+wnohang+ #:+wuntraced+
   ;; signalling / groups
   #:c-kill #:c-killpg #:c-setpgid #:c-getpgid #:c-tcsetpgrp
   ;; signal numbers commonly used
   #:+sigterm+ #:+sigkill+ #:+sigcont+ #:+sigstop+ #:+sigtstp+ #:+sigint+ #:+sigchld+))

(defpackage #:consh
  (:use #:cl #:consh.ffi)
  (:documentation
   "The consh shell image: process objects, the reaper, and shell-global
    dynamic state.")
  ;; Our parse-failure condition is named PARSE-ERROR per SPEC §2; shadow the
  ;; CL symbol of the same name so CONSH:PARSE-ERROR is ours.
  (:shadow #:parse-error)
  (:export
   ;; dynamic environment
   #:*current-directory*
   #:*shell-specials* #:spawn-stage-thread
   ;; object channels (Phase 2)
   #:channel #:make-channel #:channelp
   #:channel-put #:channel-take
   #:close-channel #:close-for-reading
   #:+channel-eof+ #:eof-p
   #:channel-closed #:channel-closed-channel #:channel-closed-object
   #:channel-closed-reason
   #:channel-capacity #:channel-count #:channel-empty-p #:channel-full-p
   #:channel-writer-closed-p #:channel-reader-closed-p
   #:stop-flag #:make-stop-flag #:stop-flag-pause #:stop-flag-resume
   #:stop-flag-paused-p
   ;; process objects
   #:process #:processp #:process-pid #:process-command
   #:process-status #:process-exit-code #:process-term-signal
   #:process-stop-signal #:process-live-p #:process-exited-p
   #:process-success-p
   ;; spawning at the shell level
   #:launch
   ;; waiting
   #:wait-process #:process-wait #:process-status-changed
   ;; reaper lifecycle
   #:start-reaper #:stop-reaper #:ensure-reaper #:reaper-running-p
   #:live-process-count #:any-children-p
   ;; signalling processes
   #:signal-process #:signal-process-group #:terminate-process #:kill-process
   ;; ---- Phase 3: parser protocol ----
   ;; lazy channel-backed object sequences
   #:object-seq #:object-seq-p #:object-seq-channel #:object-seq-thread
   #:spawn-object-seq #:emitting
   #:seq-next #:seq-take #:seq-collect #:seq-close #:do-object-seq #:take
   ;; invocation objects + registry
   #:command-invocation #:invocation-program #:invocation-arguments
   #:invocation-dialect #:make-invocation #:register-wrapper #:invocation-class-for
   #:split-flags #:flag-present-p
   ;; protocol generics
   #:parse-output #:parse-error-output #:unparse-input #:command-dialect
   #:rewrite-invocation
   ;; command-dialect probing
   #:probe-dialect #:ensure-dialect #:next-dialect #:reset-dialect-cache
   #:*dialects* #:*dialect-cache*
   ;; parse-error condition + restarts
   #:parse-error #:parse-error-command #:parse-error-raw #:parse-error-cause
   #:parse-record #:signal-parse-error
   #:use-raw-lines #:try-dialect #:define-parser
   ;; external-command failure
   #:command-failed #:command-failed-command #:command-failed-status
   #:command-failed-stderr
   ;; wrappers: ls
   #:ls-invocation #:ls-long-p #:ls-paths #:ls-directory
   #:file-info #:file-name #:file-path #:file-size #:file-mtime #:file-mode
   #:file-uid #:file-gid #:file-owner #:enrich-file
   ;; wrappers: find / cat / grep / stat
   #:find-invocation #:find-print0-p #:find-start
   #:cat-invocation #:grep-invocation #:stat-invocation
   ;; ---- Phase 4: pipeline compiler + executor ----
   #:runnable-seq
   ;; stage + pipeline objects
   #:stage #:stage-name #:external-stage #:external-stage-p #:stage-invocation
   #:stage-redirections
   #:lisp-stage #:lisp-stage-p #:stage-xform #:stage-expensive-p #:stage-parallel-p
   #:stage-generator #:generator-stage-p
   #:external #:map-stage #:filter-stage #:mapcat-stage #:emit-stage #:generator-stage
   #:pipeline #:make-pipeline #:pipeline-stages #:pipe
   ;; plumbing analysis
   #:pipeline-groups #:pipeline-plan #:compose-xforms
   ;; execution
   #:run-pipeline #:pipeline-collect #:seq-of-list
   #:pipeline-result #:pipeline-result-pipeline #:pipeline-result-seq
   #:pipeline-result-state #:pipeline-result-on-failure #:pipeline-result-elapsed
   #:pipeline-result-processes #:pipeline-result-pump-count
   ;; failure + restart
   #:pipeline-failed #:pipeline-failed-stage #:pipeline-failed-stage-index
   #:restart-stage
   ;; ---- Phase 5: jobs + cross-thread conditions ----
   #:*channel-stop-flag* #:*worker-error-hook*
   #:job #:job-id #:job-pipeline #:job-result #:job-state #:job-background-p
   #:job-on-failure #:job-stop-flag #:job-output-list #:job-output-dropped
   #:job-complete-p #:*default-job-buffer-capacity*
   #:ring-buffer #:make-ring-buffer #:ring-push #:ring-list
   #:ring-buffer-capacity #:ring-buffer-count #:ring-buffer-dropped
   #:job-events #:job-parked #:job-restarts
   #:run-job #:wait-job #:find-job #:all-jobs #:register-job
   #:fg #:bg #:stop-job #:continue-job #:kill-job
   #:debug-job #:resume-job #:take-job-events
   #:skip-object
   ;; ---- Phase 6: surface syntax ----
   #:tokenize #:parse-shell-line #:%shell-run #:shell-eval #:shell-repl
   #:shell-parse-error
   ;; builtins, $VAR expansion, globbing
   #:*builtins* #:define-builtin #:builtin #:builtin-p #:%builtin
   #:shell-exit #:shell-exit-code #:*previous-directory*
   #:%expand-vars #:glob
   #:*aliases* #:define-alias #:remove-alias
   #:*history* #:record-history #:history-count #:history-ref #:history-form
   #:history-result #:last-result #:clear-history
   #:*prompt-function* #:default-prompt #:prompt
   #:complete #:complete-line
   ;; line editor
   #:*line-history* #:record-line #:make-ledit #:ledit #:ledit-text #:ledit-point
   #:ledit-key #:ledit-complete #:read-line-edited #:interactive-terminal-p
   #:shell-repl #:main))
