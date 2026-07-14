#!/usr/bin/env bash
#
# Regenerate assets/prompt-demo.gif — the README prompt-customization demo —
# from a real consh binary.
#
# Pipeline: build a throwaway git repo (a .git/HEAD the prompt can read) with a
# spread of file sizes, drop a prompt init file under a throwaway
# XDG_CONFIG_HOME, drive an interactive consh session with assets/prompt-demo.py
# (a pty driver that records a well-paced asciinema cast), and render to a GIF
# with agg.
#
# Requires: python3, agg (both on $PATH).  Run from the repo root, or via
# `make prompt-demo` (which builds ./consh first).
set -euo pipefail

repo="$(cd "$(dirname "$0")/.." && pwd)"
consh="$repo/consh"
assets="$repo/assets"

command -v agg >/dev/null || { echo "missing required tool: agg" >&2; exit 1; }
command -v python3 >/dev/null || { echo "missing required tool: python3" >&2; exit 1; }
[ -x "$consh" ] || { echo "no consh binary at $consh — run 'make build' first" >&2; exit 1; }

work="$(mktemp -d)"
trap 'rm -rf "$work" /tmp/myapp' EXIT

# A prompt init file: bold-blue cwd, green branch (read from .git/HEAD), and a
# red [N] marker only when the last foreground command failed.
cfg="$work/config"
mkdir -p "$cfg/consh"
cat > "$cfg/consh/consh.lisp" <<'LISP'
(in-package :consh)
(setf *prompt-function*
      (lambda ()
        (let ((branch (prompt-git-branch))
              (status (prompt-exit-status)))
          (format nil "~A~@[ ~A~]~@[~A~] > "
                  (colorize (prompt-cwd-base) :bright-blue t)
                  (and branch (colorize branch :green))
                  (unless (string= status "")
                    (colorize (format nil " [~A]" status) :red t))))))
LISP

# A demo "repo" named myapp at a short path (so the paths `cd` echoes stay
# readable): a .git/HEAD the prompt reads (no real git needed), a src/ subdir,
# and files whose sizes tell a clear story for the > 1000 filter.
demo="/tmp/myapp"
rm -rf "$demo"
mkdir -p "$demo/src" "$demo/.git"
printf 'ref: refs/heads/main\n' > "$demo/.git/HEAD"
printf 'consh: a Common Lisp Unix shell where pipelines carry CLOS objects, not bytes.\n' > "$demo/README.md"
printf 'todo: ship it\n' > "$demo/notes.txt"
head -c 4096 /dev/zero | tr '\0' 'x' > "$demo/kernel.img"
printf '(defun main () (shell-repl))\n' > "$demo/src/main.lisp"

python3 "$assets/prompt-demo.py" "$consh" "$cfg" "$demo" "$assets/prompt-demo.cast"

agg --theme dracula --font-size 20 --speed 1.2 --last-frame-duration 3 \
    "$assets/prompt-demo.cast" "$assets/prompt-demo.gif"

echo "wrote $assets/prompt-demo.cast and $assets/prompt-demo.gif"
