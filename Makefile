all:

include Makefunc.mk

TOP       := $(dir $(lastword $(MAKEFILE_LIST)))
EMACS_RAW  := $(filter-out emacs-undumped, $(shell compgen -c emacs- | xargs))
ALL_EMACS  := $(strip $(sort $(EMACS_RAW)))

EMACS     ?= emacs

LOAD_PATH := -L $(TOP)
ARGS       := -Q --batch $(LOAD_PATH)
BATCH      := $(EMACS) $(ARGS)

ELS   := leaf.el
ELCS  := $(ELS:%.el=%.elc)

LOGFILE    := .make-debug.log

##################################################

all: git-hook build

git-hook:
# cp git hooks to .git/hooks
	cp -a git-hooks/* .git/hooks/

build: $(ELCS)

%.elc: %.el
	@printf "Compiling $<\n"
	-@$(BATCH) -f batch-byte-compile $<

test: # build
# If byte compile for specific emacs,
# set specify EMACS such as `EMACS=emacs-26.1 make test`.
	$(MAKE) clean --no-print-directory
	$(BATCH) -l leaf-tests.el -f cort-run-tests

localtest: $(ALL_EMACS:%=.make-debug-%)
	@echo ""
	@cat $(LOGFILE) | grep =====
	@rm $(LOGFILE)

.make-debug-%:
	EMACS=$* $(MAKE) test --no-print-directory 2>&1 | tee -a $(LOGFILE)

clean:
	-find . -type f -name "*.elc" | xargs rm
