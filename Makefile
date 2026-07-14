# Makefile for consh — build the standalone image, run tests, record the demo.
#
# Dependencies are managed with ocicl (see README).  These targets pin the ASDF
# source-registry to the project tree and the fetched ./ocicl systems, so they
# work with `--no-userinit --no-sysinit` regardless of ocicl's ~/.sbclrc setup —
# the same approach CI uses.

SBCL       ?= sbcl
LISP_FLAGS := --noinform --no-userinit --no-sysinit --non-interactive
# Point ASDF at the project (for consh.asd) and ./ocicl (for the pinned deps).
REGISTRY   := (asdf:initialize-source-registry (list :source-registry (list :tree (truename ".")) :inherit-configuration))

SOURCES := consh.asd $(wildcard src/*.lisp) $(wildcard src/wrappers/*.lisp)

.PHONY: all build deps test demo prompt-demo man clean

all: consh

## build the standalone executable at ./consh
build: consh

consh: $(SOURCES)
	$(SBCL) $(LISP_FLAGS) \
	  --eval '(require :asdf)' \
	  --eval '$(REGISTRY)' \
	  --eval '(asdf:make :consh)'
	@test -x ./consh && echo "built ./consh"

## restore pinned dependencies from ocicl.csv
deps:
	ocicl install

## run the FiveAM suite (non-zero exit on failure)
test:
	$(SBCL) $(LISP_FLAGS) \
	  --eval '(require :asdf)' \
	  --eval '$(REGISTRY)' \
	  --eval '(handler-case (asdf:load-system :consh/test) (serious-condition (e) (format *error-output* "~&LOAD FAILED: ~A~%" e) (uiop:quit 2)))' \
	  --eval '(unless (uiop:symbol-call :fiveam :run! (find-symbol "CONSH" (find-package :consh/test))) (uiop:quit 1))'

## re-record assets/demo.gif from assets/demo.exp (needs expect, asciinema, agg)
demo: consh
	./assets/record-demo.sh

## re-record assets/prompt-demo.gif from assets/prompt-demo.py (needs python3, agg)
prompt-demo: consh
	./assets/record-prompt-demo.sh

## view the manual page (man/consh.1)
man:
	man ./man/consh.1

## remove the built image
clean:
	rm -f ./consh
