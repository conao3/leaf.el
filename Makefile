all:

include Makefunc.mk

TOP         := $(dir $(lastword $(MAKEFILE_LIST)))
EMACS_RAW   := $(filter-out emacs-undumped, $(shell compgen -c emacs- | xargs))
ALL_EMACS   := $(strip $(sort $(EMACS_RAW)))

EMACS       ?= emacs

BATCH       := $(EMACS) -Q --batch -L $(TOP)

TESTFILE    := leaf-tests.el
ELS         := leaf.el
ELS           += leaf-core.el leaf-polyfill.el
ELS           += leaf-handler.el leaf-backend.el

CORTELS     := $(TESTFILE) cort.el
CORT_ARGS   := -l $(TESTFILE) -f cort-run-tests

LOGFILE     := .make-check.log

##################################################
# $(if $(findstring 22,$(shell $* --version)),[emacs-22],[else emacs-22])

all: git-hook $(ELS:.el=.elc)

git-hook:
	cp -a git-hooks/* .git/hooks/

include Makefile-check.mk

##############################
#  test on all Emacs

allcheck: $(ALL_EMACS:%=.make-check-%)
	@echo ""
	@cat $(LOGFILE) | grep =====
	@rm $(LOGFILE)

.make-check-%:
	mkdir -p .make-$*
	cp -f $(ELS) $(CORTELS) .make-$*/
	$(call EXPORT,ELS CORT_ARGS,Makefile-check.mk,.make-$*)
	EMACS=$* $(MAKE) -C .make-$* clean
	EMACS=$* $(MAKE) -C .make-$* check 2>&1 | tee -a $(LOGFILE)
	rm -rf .make-$*

##############################
#  silent `allcheck' job

test: $(ALL_EMACS:%=.make-test-%)
	@echo ""
	@cat $(LOGFILE) | grep =====
	@rm $(LOGFILE)

.make-test-%:
	mkdir -p .make-$*
	cp -f $(ELS) $(CORTELS) .make-$*/
	$(call EXPORT,ELS CORT_ARGS,Makefile-check.mk,.make-$*)
	EMACS=$* $(MAKE) -C .make-$* clean
	EMACS=$* $(MAKE) -C .make-$* check 2>&1 >> $(LOGFILE)
	rm -rf .make-$*
