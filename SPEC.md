# consh — Design Specification

A Common Lisp Unix shell with no POSIX-compliance requirement. The shell is a
Lisp image with the OS reified as objects. This document is the contract;
phases at the end define implementation order and acceptance criteria.

## 1. Core model

- Pipelines carry sequences of CLOS objects (or plists/structs), not bytes.
  Text exists only at the boundary with external processes and the terminal.
- Rendering to the terminal goes through `print-object` / presentation methods,
  customizable per type.
- Failures are conditions, not exit codes. Exit codes are translated into
  typed conditions at the boundary (§2). Restarts are the recovery mechanism.
- No quoting/word-splitting layer: arguments are Lisp values. Globbing is a
  function returning pathname objects.
- The user environment is the image: aliases are functions, the prompt is a
  function, completion is a generic function. History is a sequence of
  (form . result) pairs; results hold live objects.
- `cd` never calls `chdir(2)`. `*current-directory*` is a special variable;
  spawning resolves relative paths via `posix_spawn_file_actions_addchdir`
  (fallback: `*at` syscalls). Multiple REPLs in one image may sit in
  different directories.

## 2. Per-command parser protocol

The boundary between bytes and objects is a protocol each command wrapper
participates in. CLOS open dispatch is the point: new wrappers require no
changes to the shell core.

### Generics

```lisp
(defgeneric parse-output (command stream &key &allow-other-keys))
  ;; => lazy sequence of objects. Default method on T: lines as strings.

(defgeneric parse-error-output (command stream status))
  ;; => condition object. Default: generic COMMAND-FAILED with status + stderr.

(defgeneric unparse-input (command objects stream))
  ;; serialize objects into an external command's stdin.

(defgeneric run-and-parse (command))
  ;; wrapper may REWRITE the invocation (e.g. add --json) before running.

(defgeneric command-dialect (command))
  ;; probe & cache GNU/BSD/version dialect where output differs.
```

### Rules

- Dispatch on **invocation objects**, not command-name symbols alone.
  The sugar layer builds e.g. `ls-invocation` with parsed flags
  (`long-p`, `paths`), because `ls` and `ls -l` have different shapes.
- Format preference order: JSON flags (`ip -json`, `lsblk -J`) >
  porcelain modes (`git status --porcelain=v2`) > NUL-delimited
  (`find -print0`) > record formats (`/proc`, `passwd`) > column-scraping.
  The wrapper encodes which its tool supports and may rewrite the invocation
  to request it.
- Wrappers may **enrich**: parse names from short `ls`, then `stat()` in-process
  to return full file objects. Enrichment is the on-ramp to replacing external
  tools with native implementations without changing call sites.
- Parse failures signal `parse-error` with restarts:
  `use-raw-lines`, `try-dialect`, `define-parser`.
- `parse-output` must return a **lazy** sequence (channel-backed); `take`-style
  early termination must be able to kill the producer (§4 cancellation).
- Example of stderr/status translation: rsync exit 23 →
  `rsync-partial-transfer` (with parsed failed-file list), 24 →
  `rsync-vanished-files`.
- Wrappers are distributable as ordinary ASDF systems containing `defmethod`s
  (`wrappers/` in-tree to start).

## 3. Pipeline representation & compilation

`pipe` is a macro producing a **pipeline object** (data, not execution).

```lisp
(defclass pipeline ()       ((stages ...)))
(defclass external-stage () ((invocation ...)))
(defclass lisp-stage ()     ((function ...)))   ; object-seq -> object-seq
```

`describe` on a pipeline prints the plan: which stages are processes, which
are in-image, where parse/unparse boundaries fall.

### Plumbing analysis (compiler pass over adjacent stage pairs)

| pair | plumbing |
|---|---|
| external → external | real `pipe(2)`, kernel-to-kernel, bytes never enter Lisp |
| external → lisp | pump thread: stdout → `parse-output` → channel |
| lisp → external | pump thread: channel → `unparse-input` → stdin |
| lisp → lisp | function composition (fused) or channel (parallel) |

- `(pipe (find "/") (grep "foo"))` must cost exactly one `pipe(2)` and two
  spawns — identical plumbing to bash. Never route bytes through Lisp between
  two externals.
- **Fusion pass:** consecutive lisp-stages fuse into one thread unless a stage
  is declared expensive or parallelism is requested.

### Execution & results

- `run-pipeline` takes `:on-failure` — `:signal` (any nonzero → typed
  condition carrying stage, invocation, parsed stderr) or `:collect`.
- Restart `restart-stage`: rerun a failed segment feeding buffered boundary
  objects.
- Returns a `pipeline-result`: final object sequence (or live channel),
  per-stage statuses, timing. History stores results; old outputs are
  recoverable without re-running.

## 4. Object channels

Bounded thread-safe queue (default capacity 256) + EOF sentinel.

- `channel-put` blocks when full → **backpressure** (object-level analogue of
  the 64 KB kernel pipe buffer; memory stays flat under fast producers).
- `channel-take` blocks when empty; returns EOF sentinel after close.
- `close-for-reading`: downstream cancellation. Upstream putters observe
  `channel-closed` condition → pump thread stops reading → closes subprocess
  stdout → SIGPIPE cascade upstream. Kernel side propagates cancellation by
  SIGPIPE; Lisp side by condition; boundary stages translate.
- Backpressure doubles as **pause semantics**: one parked thread stalls the
  whole line coherently (queues fill, kernel pipes fill, writers block). No
  separate suspend machinery.

## 5. Thread architecture

Census for a running pipeline:

1. **REPL thread** — builds pipeline, calls `run-pipeline`, consumes or
   backgrounds. Does no pipeline work; stays responsive.
2. **Pump threads** — one per parse boundary and per unparse boundary. Must
   read concurrently with the process running (never after `wait`) or chatty
   processes deadlock on the 64 KB buffer. Executor guarantees this so wrapper
   authors can write naive `read-line` loops.
3. **Stderr drainers** — every external's stderr drained concurrently into a
   buffer for later `parse-error-output`. Optimization: one shared drainer
   multiplexing all stderr fds with `poll`.
4. **Lisp-stage workers** — one per unfused stage (take/transform/put loop).
5. **Reaper** — a single long-lived thread owning `waitpid` for the image.
   SIGCHLD blocked in all threads; reaper loops `waitpid(-1, WNOHANG)` woken
   via self-pipe or `sigwaitinfo`; updates process objects; notifies waiters
   via condition variables. No Lisp in signal handlers.

### Discipline

- **Ownership:** each fd / channel end has exactly one owning thread; transfers
  explicit; cleanup is local `unwind-protect` per thread body.
- **Cancellation** flows through objects (closed channels, EOF), never
  `terminate-thread`. The only async interrupt is user `C-c`, translated to
  "close terminal-facing channel + `killpg` the job".
- **Dynamic environment:** `*shell-specials*` (`*current-directory*`,
  `*environment*`, parse locale, ...) snapshotted at `run-pipeline` and
  rebound via `progv` in every worker (`spawn-stage-thread`).
- **fd hygiene:** build all pipes before spawning; parent closes its copies
  immediately; close-on-exec everywhere; missing this = "grep never sees EOF"
  hang. One tested function, not re-derived per call site.
- Pump inner loops low-consing. Foreign threads never touch channels directly;
  trampoline into Lisp-created threads.

### Cross-thread conditions

- `run-pipeline` accepts handler/restart specs installed **inside each
  worker** (`:on-parse-error :use-raw-lines` works with no cross-thread magic).
- Unhandled condition in a worker → worker parks on a condvar and posts a
  **job event** ("job 3 stopped: parse-error in stage 2"), reported at the
  prompt. Analogue of stopping on SIGTTIN.
- `(debug-job 3)` attaches: the parked worker itself calls `invoke-debugger`
  with the REPL brokering I/O; all restarts live; upstream already frozen by
  backpressure. Fix and continue.

## 6. Processes & job control

- Spawn via `posix_spawn` (CFFI binding with `file_actions` control), not
  fork/exec — the image is threaded.
- All external stages of a pipeline share one **pgid**; abort = single
  `killpg`, reaping grandchildren. Pipeline runs inside `unwind-protect` /
  `with-pipeline`, or detaches as a **job object**.
- A job = subprocess set (one pgid) + Lisp thread set + channels. `fg` =
  REPL consumes the job's terminal channel + `tcsetpgrp`; `bg` = sink channel
  buffers (or tees to ring buffer). `C-z` = `SIGTSTP` the pgid + park workers
  at next channel op via stop-flag. Job control extended over threads.
- Process objects are inspectable, signalable, holdable in variables.

## 7. Surface syntax

- A shell readtable: a bare line reads as a command form
  (`ls -l /tmp` → `(run 'ls :long t "/tmp")`); `,` or `$(...)` escapes to full
  Lisp. Reader-level sugar over real s-expressions — interactive and scripted
  use are one language.
- Deferred until the core works (Phase 6).

## 8. Implementation phases

Each phase must end with its FiveAM tests green before the next begins.

### Phase 1 — FFI + spawn + reaper
`ffi.lisp`, `reaper.lisp`. Bind `pipe`, `posix_spawn` (+file_actions incl.
addchdir if available), `waitpid`, `kill`/`killpg`, `setpgid`, `tcsetpgrp`,
fcntl CLOEXEC. Process objects with status updated by reaper.
**Accept:** spawn `true`/`false`, statuses correct via reaper; spawn 100
concurrent `sleep 0.1`, all reaped, no zombies (check via `waitpid` ECHILD);
relative-path spawn honors `*current-directory*` without process-wide chdir.

### Phase 2 — Channels
`channel.lisp`. Bounded queue, EOF sentinel, `close-for-reading`,
`channel-closed` condition, stop-flag parking hooks.
**Accept:** producer blocks at capacity; take-after-close returns sentinel;
put-after-close-for-reading signals; 2-thread ping-pong of 100k objects with
bounded memory.

### Phase 3 — Parser protocol
`invocation.lisp`, `parse.lisp`, wrappers for `ls`, `find`, `cat`, `grep`.
Default string-lines method; `ls-invocation` with enrichment (stat via FFI);
`parse-error` + restarts; dialect probe scaffold.
**Accept:** unknown command yields string lines; `ls` yields file objects with
size/mtime/owner; forced malformed input → `use-raw-lines` restart recovers.

### Phase 4 — Pipeline compiler + executor
`pipeline.lisp`, `exec.lisp`. Stage classes, plumbing analysis, fusion pass,
pump threads, stderr drainers, `pipeline-result`, `:on-failure` modes,
`restart-stage`, `describe` plan output.
**Accept:** external→external pair verified to use one kernel pipe (no Lisp
byte traffic — assert no pump thread created); `(take 5 (pipe (find "/") ...))`
terminates promptly and `find` is dead; `yes | head`-equivalent doesn't hang
(SIGPIPE path); failing middle stage with `:signal` delivers typed condition
naming the stage; stderr-heavy process doesn't deadlock.

### Phase 5 — Jobs + cross-thread conditions
`jobs.lisp`. Job objects, pgids, fg/bg, C-z path, job events, `debug-job`
attach, handler-spec installation in workers.
**Accept:** background a pipeline, foreground it, output intact; parse-error in
worker parks job and surfaces event; `debug-job` presents restarts and
`continue` resumes the frozen line end-to-end.

### Phase 6 — Surface
Readtable sugar, prompt function, history of (form . result), completion
generic. Out of scope for acceptance until Phases 1–5 are green.

## 9. Non-goals

- POSIX sh compatibility, `set -e`/pipefail emulation, string-based `eval`.
- Portability beyond SBCL/Linux (macOS later; Windows never).
- Performance beyond "external→external as fast as bash" until Phase 6.
