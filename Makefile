TOP       := $(dir $(lastword $(MAKEFILE_LIST)))

EMACS     ?= emacs

LOAD_PATH := -L $(TOP)
BATCH     := $(EMACS) -Q --batch $(LOAD_PATH)

ELS   := leaf.el
ELCS  := $(ELS:.el=.elc)

include Makefunc.mk

all: build

build: $(ELCS)

%.elc: %.el
	$(EMACS) --version
	@printf "Compiling $<\n"
	-@$(BATCH) -f batch-byte-compile $<

test:
	$(EMACS) --version
	$(BATCH) --eval "(require 'ert)"; \
	  if [ $$? -ne 0 ]; then \
	    echo "emacs-noert"; \
	    $(BATCH) -l leaf-tests-noert.el; \
	  else \
	    echo "emacs-ert"; \
	    $(BATCH) -l leaf-tests.el -f ert-run-tests-batch-and-exit; \
	  fi

localtest:
	$(call ECHO_MAGENTA, "test by emacs-22.1")
	EMACS=emacs-22.1 make test

	@echo "\n"
	$(call ECHO_MAGENTA, "test by emacs-26.1")
	EMACS=emacs-26.1 make test

	@echo "\n"
	$(call ECHO_CYAN, "localtest completed!!")
	@echo "\n"

clean:
	-find . -type f -name "*.elc" | xargs rm
