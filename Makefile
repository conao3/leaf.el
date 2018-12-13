all:

include Makefunc.mk

TOP        := $(dir $(lastword $(MAKEFILE_LIST)))
EMACS_RAW  := $(filter-out emacs-undumped, $(shell compgen -c emacs- | xargs))
ALL_EMACS  := $(strip $(sort $(EMACS_RAW)))

EMACS      ?= emacs

LOAD_PATH  := -L $(TOP)
ARGS       := -Q --batch $(LOAD_PATH)
BATCH      := $(EMACS) $(ARGS)

CORTELS    := leaf-tests.el cort.el
ELS        := leaf.el leaf-backends.el
ELCS       := $(ELS:%.el=%.elc)

LOGFILE    := .make-test.log

##################################################

# $(if $(findstring 22,$(shell $* --version)),[emacs-22],[else emacs-22])

all: git-hook build

git-hook:
# cp git hooks to .git/hooks
	cp -a git-hooks/* .git/hooks/

build: $(ELCS)

%.elc: %.el
	@printf "Compiling $<\n"
	@$(BATCH) -f batch-byte-compile $<

check: # build
# If byte compile for specific emacs,
# set specify EMACS such as `EMACS=emacs-26.1 make check`.
	$(MAKE) clean --no-print-directory
	$(BATCH) -l leaf-tests.el -f cort-run-tests

allcheck: $(ALL_EMACS:%=.make-check-%)
	@echo ""
	@cat $(LOGFILE) | grep =====
	@rm $(LOGFILE)

.make-check-%:
	EMACS=$* $(MAKE) check --no-print-directory 2>&1 | tee -a $(LOGFILE)

# silent `allcheck' job
test: $(ALL_EMACS:%=.make-test-%)
	@echo ""
	@cat $(LOGFILE) | grep =====
	@rm -rf $(LOGFILE)

.make-test-%:
	mkdir -p .make-$*
	cp -f $(ELS) $(CORTELS) .make-$*/
	cd .make-$*; $* -Q --batch -L `pwd` -f batch-byte-compile $(ELS)
	cd .make-$*; $* -Q --batch -L `pwd` -l leaf-tests.el -f cort-run-tests 2>&1 >> ../$(LOGFILE)
	rm -rf .make-$*

updatecort:
	cp -f ../cort.el/cort.el ./

clean:
	-find . -type f -name "*.elc" | xargs rm
	-rm -rf .make-*
