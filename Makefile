all:

include Makefunc.mk

TOP         := $(dir $(lastword $(MAKEFILE_LIST)))
EMACS_RAW   := $(filter-out emacs-undumped, $(shell compgen -c emacs- | xargs))
ALL_EMACS   := $(strip $(sort $(EMACS_RAW)))

EMACS       ?= emacs

BATCH       := $(EMACS) -Q --batch -L $(TOP)
BATCH_LOCAL  = $* -Q --batch -L `pwd`

TESTFILE    := leaf-tests.el
ELS         := leaf.el leaf-backends.el
ELCS        := $(ELS:.el=.elc)
CORTELS     := $(TESTFILE) cort.el
CORT_ARGS   := -l $(TESTFILE) -f cort-run-tests

LOGFILE     := .make-check.log

TOUCH_TIME     := 201001010000
RECOMPILE_SEXP := --eval '(byte-recompile-directory "./")'


##################################################
# $(if $(findstring 22,$(shell $* --version)),[emacs-22],[else emacs-22])

all: git-hook build

git-hook:
	cp -a git-hooks/* .git/hooks/

##############################
#  byte-compile job

build: $(ELCS)

$(ELCS): $(ELS)
	$(EMACS) --version
	touch -t $(TOUCH_TIME) $(ELCS)
	$(BATCH) $(RECOMPILE_SEXP)

##############################
#  simple test job

# If you want to run specify EMACS,
# run `make` such as `EMACS=emacs-26.1 make check`.

check: build
	$(BATCH) $(CORT_ARGS)

##############################
#  test on all Emacs

allcheck: $(ALL_EMACS:%=.make-check-%)
	@echo ""
	@cat $(LOGFILE) | grep =====
	@rm $(LOGFILE)

.make-check-%:
	mkdir -p .make-$*
	cp -f $(ELS) $(CORTELS) .make-$*/
	cd .make-$*; touch -t $(TOUCH_TIME) $(ELCS)
	cd .make-$*; $(BATCH_LOCAL) $(RECOMPILE_SEXP)
	cd .make-$*; find . -name "*.elc" | xargs rm -rf
	cd .make-$*; $(BATCH_LOCAL) $(CORT_ARGS) 2>&1 | tee -a ../$(LOGFILE)
	@rm -rf .make-$*

##############################
#  silent `allcheck' job

test: $(ALL_EMACS:%=.make-test-%)
	@echo ""
	@cat $(LOGFILE) | grep =====
	@rm $(LOGFILE)

.make-test-%:
	mkdir -p .make-$*
	cp -f $(ELS) $(CORTELS) .make-$*/
	cd .make-$*; touch -t $(TOUCH_TIME) $(ELCS)
	cd .make-$*; $(BATCH_LOCAL) $(RECOMPILE_SEXP)
	cd .make-$*; find . -name "*.elc" | xargs rm -rf
	cd .make-$*; $(BATCH_LOCAL) $(CORT_ARGS) 2>&1 >> ../$(LOGFILE)
	@rm -rf .make-$*

##############################
#  other jobs

clean:
	-find . -name "*.elc" | xargs rm -rf
	-rm -rf .make-*
