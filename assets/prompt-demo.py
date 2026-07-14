#!/usr/bin/env python3
"""Drive consh under a pty and emit a well-paced asciinema v2 cast.

Invoked by assets/record-prompt-demo.sh, which builds the demo fixtures and
renders the cast to a GIF with agg.  Usage:

    prompt-demo.py CONSH XDG_CONFIG_HOME DEMO_DIR OUT.cast

Unlike `asciinema rec --headless`, this records an output event for every small
chunk, with realistic inter-keystroke gaps, so agg renders smooth typing and
holds each result long enough to read.  It also fixes the pty to 92x24 so the
longer Lisp forms never wrap.
"""
import os, sys, pty, time, json, select, struct, fcntl, termios, random

CONSH = sys.argv[1]   # absolute path to the consh binary (we chdir before exec)
CFG   = sys.argv[2]   # XDG_CONFIG_HOME holding consh/consh.lisp (the prompt)
DEMO  = sys.argv[3]   # directory to start in (a fake git repo)
OUT   = sys.argv[4]   # asciinema v2 cast to write
COLS, ROWS = 92, 24

random.seed(7)  # deterministic typing jitter

# The story. Each item is (text, pause_after_seconds[, mode]).  mode defaults to
# "type" (type TEXT then Enter); "suggest" types TEXT as a prefix, waits for the
# autosuggestion ghost, then presses Right to accept it before Enter.
SCRIPT = [
    ("ls", 1.7),                                       # a bare command auto-tables its objects
    ("(pipeline-collect (pipe (ls) (:filter (lambda (f) (> (file-size f) 1000)))))", 2.0),
    ("seq 1 5 | grep 3", 1.7),                         # a pipe carries objects -> grep-match table
    ("false", 1.3),                                    # a failure lights the red [1] marker
    ("ls README.md", 1.7),                             # success clears it (and seeds history)
    ("ls R", 1.6, "suggest"),                          # autosuggestion: ghost "EADME.md", Right accepts
    ("cd src", 1.3),                                   # the prompt's cwd tracks
    ("cd /tmp", 1.6),                                  # leaving the repo drops the git branch
    ("cd -", 2.0),                                     # coming back restores it
]

master, slave = pty.openpty()
# Tell consh the window is 92x24 so long Lisp forms don't wrap.
fcntl.ioctl(slave, termios.TIOCSWINSZ, struct.pack("HHHH", ROWS, COLS, 0, 0))

pid = os.fork()
if pid == 0:
    os.setsid()
    os.dup2(slave, 0); os.dup2(slave, 1); os.dup2(slave, 2)
    env = dict(os.environ, XDG_CONFIG_HOME=CFG, TERM="xterm-256color",
               COLUMNS=str(COLS), LINES=str(ROWS))
    os.chdir(DEMO)
    os.execve(CONSH, ["consh"], env)
os.close(slave)

t0 = time.time()
events = []

def drain(window):
    """Read for `window` seconds, recording a timestamped event per chunk."""
    end = time.time() + window
    while True:
        remaining = end - time.time()
        if remaining <= 0:
            break
        r, _, _ = select.select([master], [], [], min(remaining, 0.05))
        if r:
            try:
                data = os.read(master, 65536)
            except OSError:
                break
            if not data:
                break
            events.append([round(time.time() - t0, 4), "o",
                           data.decode("utf-8", "replace")])

drain(1.4)  # banner + first prompt
for entry in SCRIPT:
    text, pause = entry[0], entry[1]
    mode = entry[2] if len(entry) > 2 else "type"
    for ch in text:
        os.write(master, ch.encode())
        drain(0.06 + random.uniform(0, 0.05))    # keystroke echo + human jitter
    if mode == "suggest":
        drain(0.9)                               # let the dim autosuggestion appear
        os.write(master, b"\x1b[C")              # Right arrow accepts the suggestion
        drain(0.6)
    time.sleep(0.35)
    os.write(master, b"\r")
    drain(pause)
os.write(master, b"exit\r")
drain(0.8)

os.close(master)
try:
    os.waitpid(pid, 0)
except ChildProcessError:
    pass

header = {"version": 2, "width": COLS, "height": ROWS,
          "timestamp": 0, "env": {"TERM": "xterm-256color"}}
with open(OUT, "w") as f:
    f.write(json.dumps(header) + "\n")
    for e in events:
        f.write(json.dumps(e) + "\n")
print(f"wrote {OUT}: {len(events)} events, {events[-1][0]:.1f}s")
