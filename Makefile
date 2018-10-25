TOP       := $(dir $(lastword $(MAKEFILE_LIST)))

EMACS     := emacs
LOAD_PATH := -L $(TOP)
BATCH     := $(EMACS) -Q --batch $(LOAD_PATH)

ELS   := leaf.el
ELCS  := $(ELS:.el=.elc)

all: build

build: $(ELCS)

%.elc: %.el
	@printf "Compiling $<\n"
	-@$(BATCH) -f batch-byte-compile $<

test:
	$(BATCH) --eval "(require 'ert)"; \
	  if [ $$? -ne 0 ]; then \
	    $(BATCH) -l leaf-tests-noert.el; \
	  else \
	    $(BATCH) -l leaf-tests.el -f ert-run-tests-batch-and-exit; \
	  fi

clean:
	-find . -type f -name "*.elc" | xargs rm
