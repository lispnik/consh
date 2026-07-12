# CLAUDE.md — consh (Common Lisp Unix shell, non-POSIX)

## What this project is

A Unix shell implemented as a Common Lisp image. Not POSIX-compliant by design.
Pipelines carry CLOS objects, not bytes; external commands are wrapped by
generic-function parsers; failures are conditions with restarts; jobs are
objects combining subprocesses and Lisp threads.

Read `SPEC.md` before writing any code. It is the authoritative design.
Implement in the phase order given there. Do not skip ahead to interactive
features before the pipeline core is solid.

## Toolchain

- **Implementation:** SBCL only. SBCL-specific code (`sb-thread`, `sb-ext`,
  `sb-introspect`) is fine and expected. No portability layers for threading
  beyond what already exists in deps.
- **Dependencies:** managed with **ocicl** (not Quicklisp). Add deps to the
  `.asd` and run `ocicl install`.
- **System definition:** ASDF, one system `consh`, test system `consh/test`.
- **FFI:** CFFI. Prefer `posix_spawn` bindings over fork/exec (threaded image).
  Group all raw syscall bindings in `src/ffi.lisp`.
- **Tests:** FiveAM. Every phase in SPEC.md lists acceptance tests; implement
  them as FiveAM tests in `t/`. Run with:
  `sbcl --non-interactive --eval '(asdf:test-system :consh)'`

## Layout

```
consh.asd
src/
  packages.lisp
  ffi.lisp          ; CFFI bindings: pipe, posix_spawn, waitpid, killpg, tcsetpgrp
  channel.lisp      ; bounded object channels
  invocation.lisp   ; command-invocation classes, wrapper registry
  parse.lisp        ; parse-output / parse-error-output / unparse-input generics
  pipeline.lisp     ; pipe macro, stage classes, plumbing compiler
  exec.lisp         ; segment executor, pump threads, stderr drainers
  reaper.lisp       ; single waitpid thread, process objects
  jobs.lisp         ; job objects, fg/bg, debug-job
  wrappers/         ; one file per wrapped command (ls.lisp, git.lisp, ...)
t/
  ...               ; FiveAM suites mirroring src/
```

## Conventions

- Plain CLOS + generic functions. No `defmethod` on symbols where an
  invocation class is called for (see SPEC.md §2).
- Every thread body wraps its work in `unwind-protect` and closes exactly the
  fds/channel-ends it owns. Never call `sb-thread:terminate-thread` in
  product code.
- Dynamic variables that must propagate to worker threads go in
  `*shell-specials*` (see SPEC.md §5). Add to that list, never spawn naked
  `make-thread` in pipeline code — use `spawn-stage-thread`.
- Conditions: define them in the file that signals them. Every external-command
  failure must be a subtype of `command-failed`.
- No `chdir` anywhere. Ever. `*current-directory*` + spawn-time file actions.
- Keep pump-thread inner loops low-consing: reuse buffers, hand off objects.

## Workflow

- REPL-driven where possible: load the system, exercise forms, then freeze
  behavior into tests. But every change must end with the FiveAM suite green.
- When adding a command wrapper, prefer structured output flags
  (`--json`, `--porcelain`, `-print0`) over scraping human output; record the
  dialect-probe logic per SPEC.md §2.
- Integration tests that spawn real processes may only depend on coreutils
  present in a default Ubuntu container (`ls`, `cat`, `grep`, `find`, `sleep`,
  `true`, `false`, `head`).
- Commit messages: short imperative subject, body explains the design
  decision if any was made.
