TOP       := $(dir $(lastword $(MAKEFILE_LIST)))

EMACS     ?= emacs
LOAD_PATH := -L $(TOP)
BATCH     := $(EMACS) -Q --batch $(LOAD_PATH)

ELS   := leaf.el
ELCS  := $(ELS:.el=.elc)

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
	    $(BATCH) -l leaf-tests-noert.el; \
	  else \
	    $(BATCH) -l leaf-tests.el -f ert-run-tests-batch-and-exit; \
	  fi

localtest:
#	@echo -e '\e[1;34mBuilding\e[0m'
	echo -e '\e[1;31m hoge hoge \e[m'
	EMACS=emacs-22.1 make test
	EMACS=emacs-26.1 make test

clean:
	-find . -type f -name "*.elc" | xargs rm
