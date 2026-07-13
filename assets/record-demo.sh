#!/usr/bin/env bash
#
# Regenerate assets/demo.gif — the README demo — from a real consh binary.
#
# Pipeline: build a throwaway demo directory with a spread of file sizes, drive
# an interactive consh session with assets/demo.exp, record it headless with
# asciinema, widen the cast to 100 cols, and render to a GIF with agg.
#
# Requires: expect, asciinema, agg (all on $PATH).  Run from the repo root, or
# via `make demo` (which builds ./consh first).
set -euo pipefail

repo="$(cd "$(dirname "$0")/.." && pwd)"
consh="$repo/consh"
assets="$repo/assets"

for tool in expect asciinema agg; do
    command -v "$tool" >/dev/null || { echo "missing required tool: $tool" >&2; exit 1; }
done
[ -x "$consh" ] || { echo "no consh binary at $consh — run 'make build' first" >&2; exit 1; }

# A fresh demo directory named consh-demo (the basename shows up in the prompt),
# with files whose sizes tell a clear story for the > 1000 bytes filter.
demo="$(mktemp -d)/consh-demo"
mkdir -p "$demo/src"
printf 'consh: a Common Lisp Unix shell where pipelines carry CLOS objects, not bytes.\n' > "$demo/README.md"
printf 'todo: ship it\n' > "$demo/notes.txt"
head -c 4096 /dev/zero | tr '\0' 'x' > "$demo/kernel.img"
printf '(defun main () (shell-repl))\n' > "$demo/src/main.lisp"

raw="$(mktemp -u).cast"
(
  cd "$demo"
  asciinema rec --headless -c "expect $assets/demo.exp $consh" \
    --idle-time-limit 1.5 --overwrite "$raw"
)

# Drop asciinema's own spawn/exit bookkeeping lines and widen the terminal so
# the longer Lisp forms render on one row (agg lays out at the header size).
grep -v '"spawn ' "$raw" \
  | sed 's/"cols":80/"cols":100/; s/"rows":24/"rows":22/' \
  > "$assets/demo.cast"

agg --theme dracula --font-size 20 --speed 1.15 --last-frame-duration 3 \
    "$assets/demo.cast" "$assets/demo.gif"

echo "wrote $assets/demo.cast and $assets/demo.gif"
