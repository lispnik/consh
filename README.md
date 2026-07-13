# consh

[![CI](https://github.com/lispnik/consh/actions/workflows/ci.yml/badge.svg)](https://github.com/lispnik/consh/actions/workflows/ci.yml)

A Unix shell implemented as a Common Lisp image — **pipelines carry CLOS
objects, not bytes.**

<p align="center">
  <img src="assets/demo.gif" width="820"
       alt="A consh session: a seq|grep pipeline whose grep hands back a match object (line number + text), then `ls` yielding file-info objects, filtering those objects by a Lisp predicate on file-size, folding find's enriched output into a byte total, and rendering an object stream as an aligned table — all at one prompt.">
</p>

Text exists only at the boundary with external processes and the terminal.
Inside the image, `ls` yields file *objects*, a pipeline stage is a Lisp
function, a command failure is a *condition* with restarts, and a job is a live
object combining subprocesses and Lisp threads. It is deliberately **not**
POSIX-compliant: there is no string `eval`, no word-splitting layer — arguments
are Lisp values and globbing returns pathnames.

Built on SBCL in the phase order of [`SPEC.md`](SPEC.md), each phase ending with
its FiveAM suite green (**1265 checks**).

```
                 bytes                         objects
   ┌────────┐  kernel pipe  ┌────────┐  parse  ┌────────────┐  Lisp fn  ┌─────────┐
   │  find  │ ────────────▶ │  grep  │ ──────▶ │ grep-match │ ────────▶ │ :filter │ ─▶ results
   └────────┘               └────────┘         └────────────┘           └─────────┘
        └──────── one pgid, killpg to abort ────────┘        └── fused, one thread ──┘
```


## Installation

consh needs **[SBCL](https://www.sbcl.org/)** (developed on 2.6.5; macOS arm64
and Linux) and **[ocicl](https://github.com/ocicl/ocicl)** for its two
dependencies (`cffi`, `fiveam`).

**1. Install the prerequisites.**

```sh
# macOS / Linux / WSL — Homebrew
brew install sbcl ocicl

# Debian / Ubuntu
sudo apt install sbcl
curl -fsSL https://ocicl.github.io/ocicl/deb-repo/ocicl-archive-keyring.gpg \
  | sudo tee /usr/share/keyrings/ocicl-archive-keyring.asc > /dev/null
echo "deb [signed-by=/usr/share/keyrings/ocicl-archive-keyring.asc] https://ocicl.github.io/ocicl/deb-repo stable main" \
  | sudo tee /etc/apt/sources.list.d/ocicl.list
sudo apt update && sudo apt install ocicl

# Fedora / RHEL
sudo dnf install sbcl
sudo dnf config-manager addrepo --from-repofile=https://ocicl.github.io/ocicl/rpm-repo/ocicl.repo
sudo dnf install ocicl
```

```sh
ocicl setup            # one-time: registers the ocicl runtime in ~/.sbclrc
```

**2. Get consh and its dependencies.**

```sh
git clone https://github.com/lispnik/consh.git
cd consh
ocicl install          # restore deps pinned in ocicl.csv into ./ocicl/
```

**3. Build the executable** (optional — see "Building a standalone executable"),
and put it on your `PATH`:

```sh
make                                                 # -> ./consh (or: make build)
install -m 755 consh ~/.local/bin/consh              # or /usr/local/bin (sudo)
consh
```

The [`Makefile`](Makefile) also has `make test` (run the FiveAM suite),
`make deps` (`ocicl install`), and `make demo` (re-record the GIF above).

**Or use it from a REPL** — which is how the examples below run (`=>` shows the
real returned value):

```sh
sbcl                   # run inside the cloned repo
```
```lisp
(asdf:load-system :consh)
(in-package :consh)
```

Run the tests with `make test` (or
`sbcl --non-interactive --eval '(asdf:test-system :consh)'`).


## The idea: objects, not bytes

A parser protocol turns each command's output into objects at the boundary.
`ls` prints only names, so its wrapper parses the names and then `stat()`s each
in-process to return enriched file objects — the on-ramp to replacing external
tools with native ones without changing call sites:

```lisp
;; ls ENRICHES bare names into file objects (size / mtime / owner via stat)
(let ((inv (make-instance 'ls-invocation :directory #P"/tmp/consh-demo/")))
  (seq-collect (parse-output inv (make-string-input-stream
                                  (format nil "readme.txt~%data.bin~%")))))
=> (#<FILE-INFO readme.txt 5 bytes mkennedy>
    #<FILE-INFO data.bin 10 bytes mkennedy>)

;; an unregistered command falls back to lines of text
(seq-collect (parse-output (make-invocation "tr")
                           (make-string-input-stream (format nil "line one~%line two~%"))))
=> ("line one" "line two")
```

Adding a wrapper is just `defmethod`s — no change to the shell core.

A **presentation layer** renders any object stream as an aligned table; each
wrapped type advertises its columns via the `table-columns` generic:

```lisp
(table (pipeline-collect (pipe (ls))))
NAME        SIZE  OWNER
----------  ----  --------
kernel.img  4096  mkennedy
notes.txt     14  mkennedy      ; numeric columns right-align automatically
```


## Pipelines

`pipe` builds a pipeline **object** (data, not execution). The compiler groups
adjacent stages so the plumbing matches bash where it should, and goes beyond it
where objects help.

```lisp
;; external | external is ONE real pipe(2) — bytes go kernel-to-kernel,
;; never through Lisp. Identical cost to bash.
(pipeline-collect
 (make-pipeline (list (external "sh" "-c" "printf 'foo\\nbar\\nfoobar\\n'")
                      (external "grep" "foo"))))
=> (#<GREP-MATCH 1 "foo"> #<GREP-MATCH 3 "foobar">)   ; grep -n enriches the tail

;; external -> an in-image Lisp stage (map / filter / mapcat), run in a thread
(pipeline-collect
 (make-pipeline (list (external "sh" "-c" "printf 'a\\nb\\nc\\n'")
                      (map-stage #'string-upcase))))
=> ("A" "B" "C")

;; CLOS objects cross the boundary, not text: ls | keep files bigger than 5 bytes
(pipeline-collect
 (make-pipeline (list (external (make-invocation "ls" "/tmp/consh-demo"))
                      (filter-stage (lambda (f) (> (file-size f) 5))))))
=> file-info objects for data.bin (10) and logs/ (64)

;; imperative stages when map/filter/mapcat don't fit: emit-stage hands your
;; body an `emit` function you call 0+ times (here, a stateful running total);
;; generator-stage is a pure-Lisp source.
(let ((sum 0))
  (pipeline-collect
   (make-pipeline (list (external "seq" "1" "5")
                        (emit-stage (lambda (n emit)
                                      (funcall emit (incf sum (parse-integer n)))))))))
=> (1 3 6 10 15)
```

Output is **lazy**. Taking a prefix cancels the producer — and kills the
external process (SIGPIPE + `killpg`), leaving no zombies:

```lisp
;; take 5 from an infinite `yes`, then it's gone
(take 5 (make-pipeline (list (external "yes"))))
=> ("y" "y" "y" "y" "y")
```

`describe` prints the compiled plan — which stages are processes, where the
kernel pipes and parse/unparse boundaries fall, which Lisp stages fused:

```lisp
(describe (pipe (find "/") (grep "foo") (:map #'identity)))
Pipeline of 3 stages:
  [external] find -> grep  (kernel-pipe)
      == parse boundary ==
  [lisp] map
  head: stdin   tail: channel
```

Failures are typed conditions, not exit codes. Under `:on-failure :signal` a
failing stage raises a `pipeline-failed` naming the stage and carrying its
parsed stderr; the `restart-stage` restart reruns a corrected pipeline. Wrappers
translate exit codes: `grep` exiting 1 ("no match") is *not* an error.

**Native, in-image stages** do what `grep`/`cat`/`sort`/`uniq` do with no
subprocess — over the object stream, so `sort` orders by a *slot*, not by text.
`:sort` is a barrier (it buffers to reorder); `:grep`/`:uniq` stream; `:cat` is a
source:

```lisp
;; biggest files first, then just their names — all in-image, no fork
(pipeline-collect (pipe (ls) (:sort :key #'file-size) (:map #'file-name)))
=> ("notes.txt" "README.md" "src" "kernel.img")

;; :cat | :grep | :uniq, native end to end
(pipeline-collect (pipe (:cat "log.txt") (:grep "warn") (:sort) (:uniq)))
```

Backgrounding (`&`) makes a **job**; the `jobs`, `fg`, `bg`, `wait`, and `kill`
builtins drive the job objects (`kill %1` reclaims the whole process group;
`kill -9 PID` signals a bare pid). On a real tty, `fg` performs a proper
`tcsetpgrp` handoff — the job's process group becomes the terminal's foreground
group (so C-c/C-z reach the job, not the shell), and the shell reclaims the
terminal when the job finishes or stops (SIGTTOU-safe). Under a pipe or without a
tty it all degrades to a no-op.


## Processes & object channels

The lower layers are usable on their own. Processes are inspectable, signalable
objects reaped by a single `waitpid` thread:

```lisp
(let ((p (launch "true")))  (wait-process p)
  (list (process-status p) (process-exit-code p)))
=> (:EXITED 0)
```

Channels are bounded, thread-safe object queues with backpressure and EOF —
the object-level analogue of a kernel pipe:

```lisp
(let ((ch (make-channel :capacity 4)) (obj (list :a :b)))
  (channel-put ch obj) (close-channel ch)
  (list (eq obj (channel-take ch))     ; same object back — identity preserved
        (eof-p (channel-take ch))))    ; then EOF
=> (T T)
```


## Jobs & interactive conditions

A job = a subprocess set (one pgid) + a Lisp thread set + channels. `bg`/`fg`,
and `C-z` (`stop-job`) work over *both* the processes (SIGTSTP) and the Lisp
workers (parked at their next channel op via a shared stop-flag):

```lisp
;; background a pipeline, then foreground it — output intact
(fg (run-job (make-pipeline (list (external "sh" "-c" "printf 'a\\nb\\nc\\n'")))
             :background t))
=> ("a" "b" "c")
```

A background job's output goes into a **bounded ring buffer** (scrollback), so an
unbounded producer like `yes &` keeps only the most recent objects instead of
growing without limit; `job-output-dropped` reports how many were shed. Each
external's stderr is drained fully (no deadlock) but retained up to a cap.

The headline: an **unhandled condition in a worker parks the job instead of
crashing it**, with the stack — and therefore all restarts — intact. You inspect
it at the prompt and `debug-job` resumes the frozen line end-to-end, running the
restart in the worker's own context (the shell analogue of stopping on SIGTTIN):

```lisp
;; a parse worker hit a malformed record 'OOPS'; the whole line froze:
job-state:     :PARKED
job-events:    "job 1 parked: PARSE-ERROR in parse stage"
job-restarts:  (USE-RAW-LINES TRY-DIALECT DEFINE-PARSER ABORT)

;; attach and pick a restart — the pipeline resumes and completes:
(debug-job job :restart 'use-raw-lines)
(fg job)
=> (("a" . "1") "OOPS" ("c" . "3"))
```


## Surface syntax

A bare line reads as a command; a line starting with `(` is full Lisp; `,` and
`$(...)` escape to Lisp inside a command; `|` builds a pipeline and `&`
backgrounds it. Interactive and scripted use are one language because the
surface just desugars to the pipeline/job forms above.

```lisp
(shell-eval "echo hello")                    => ("hello")
(shell-eval "seq 1 5 | grep 3")              => (#<GREP-MATCH 3 "3">)  ; grep -n enriches
(shell-eval "echo $HOME/notes")              => ("/home/you/notes")  ; $VAR expands
(shell-eval "cat *.txt")                     ; globs to matching pathnames
(shell-eval "sort < in.txt > out.txt")       ; < > >> 2> redirections
(shell-eval "echo $(string-upcase \"hi\")")  => ("HI")     ; $() escapes to Lisp
(shell-eval "(+ 40 2)")                      => 42         ; ( ... ) is Lisp

;; the desugaring is just s-expressions:
(parse-shell-line "find / | grep foo &")
=> (%SHELL-RUN (LIST (EXTERNAL "find" "/") (EXTERNAL "grep" "foo")) :BACKGROUND T)
```

Builtins run in the image: `cd` (sets `*current-directory*`, never `chdir(2)`),
`pwd`, `export`/`unset`, `alias`/`unalias`, `jobs`/`fg`/`bg`, `history`, `exit`.
The user environment is the image: aliases, the prompt (`*prompt-function*`),
history of `(form . result)` pairs whose results hold the live objects, and
completion as a generic function:

```lisp
(complete :symbol "map" :package :cl)  => ("map" "map-into" "mapc" "mapcan" "mapcar" ...)
(complete :command "gr")               ; registered wrappers + $PATH executables
(complete :path "re" :directory d)     ; filesystem entries
```

On an interactive terminal the REPL uses a line editor with **Tab completion**,
**Up/Down history**, and Emacs-ish keys (`^A`/`^E`/`^K`/`^U`, arrows); it falls
back to plain `read-line` for pipes and scripts. **Ctrl-C** aborts the current
line, or tears down a running foreground job; **Ctrl-D** exits.


## How it's built

| File | Phase | What it does |
|---|---|---|
| `src/ffi.lisp` | 1 | CFFI: `pipe`, `posix_spawn` (+ file actions incl. per-child `chdir`), `waitpid`, `kill`/`killpg`, `stat`, `getpwuid` |
| `src/reaper.lisp` | 1 | Process objects; the single, **SIGCHLD-driven** `waitpid` reaper thread; `*current-directory*` (no process-wide `chdir`, ever) |
| `src/channel.lisp` | 2 | Bounded object channels: backpressure, EOF, downstream cancellation, stop-flag parking |
| `src/invocation.lisp`, `parse.lisp`, `dialect.lisp`, `wrappers/` | 3 | Per-command parser protocol; lazy channel-backed object sequences; `parse-error` restarts; GNU/BSD **dialect probing/translation** (`stat` picks `-c`/`-f`; `sed -i`/`date -d @` rewritten to BSD); `ls`/`find` **stat-enrich** to `file-info`, `grep -n` → `grep-match`, `df` → `filesystem`, `wc` → `wc-count`, `du` → `du-entry`, `git status`, `ps`, `lsblk -J` **JSON** → `block-device` |
| `src/pipeline.lisp`, `exec.lisp` | 4 | `pipe` macro, plumbing/fusion compiler, pump threads, stderr drainers, `pipeline-result`, cancellation; native in-image `grep`/`cat`/`sort`/`uniq` stages |
| `src/jobs.lisp` | 5 | Job objects, fg/bg, C-z, job events, `debug-job` cross-thread conditions |
| `src/surface.lisp` | 6 | Reader sugar, prompt function, history, completion, aliases, job-control builtins with real tcsetpgrp terminal handoff |
| `src/present.lisp` | — | Presentation layer: `table` renders an object stream as an aligned grid via each type's `table-columns` |

Thread discipline (SPEC §5): one reaper for the image, woken by a minimal
SIGCHLD handler (which only signals a semaphore — all `waitpid` work runs in the
reaper thread, so children are reaped in well under a millisecond); one pump
thread per parse/unparse boundary; one stderr drainer per external; one worker
per unfused Lisp stage. Cancellation flows through objects (closed channels, EOF) and
`killpg` — never `terminate-thread`. Every pipe end is close-on-exec and the
parent closes its child-side copies immediately. `stat`/`fcntl` go through
`sb-posix` for ABI correctness (a naive CFFI `fcntl` corrupts args on Darwin
arm64).


## Testing

```sh
sbcl --non-interactive --eval '(asdf:test-system :consh)'
```

908 FiveAM checks across suites mirroring `src/` — including the SPEC acceptance
tests for every phase (100 concurrent `sleep`s all reaped; `external→external`
uses exactly one kernel pipe; `yes | head` doesn't hang; a failing middle stage
names itself; a stderr flood doesn't deadlock; C-z freezes and resumes intact;
`debug-job` resumes a parked line). Integration tests depend only on coreutils.


## Building a standalone executable

The `consh` system is wired for ASDF's `program-op`, so it dumps a self-contained
image with a REPL entry point (`consh:main`).  `make` wraps this:

```console
$ make                                                  # -> ./consh
$ printf 'echo hi from the image\nseq 1 3 | grep 2\n(+ 40 2)\n' | ./consh
consh — a Common Lisp Unix shell (objects, not bytes). Ctrl-D to exit.
consh consh> hi from the image
consh consh> 2
consh consh> 42
```

(The FFI re-resolves its cached libc symbol pointers on image startup via
`sb-ext:*init-hooks*`, so the dumped executable spawns processes correctly
despite ASLR.)


## Non-goals

POSIX `sh` compatibility, `set -e`/pipefail emulation, string-based `eval`, and
portability beyond SBCL. See [`SPEC.md`](SPEC.md) for the full design.
