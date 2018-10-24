EMACS := emacs
BATCH := $(EMACS) -Q --batch
ELS   := leaf.el
ELCS  := $(ELS:.el=.elc)

all: clean build

build: $(ELCS)

%.elc: %.el
	@printf "Compiling $<\n"
	-@$(BATCH) -f batch-byte-compile $<

test:
	$(BATCH) --eval "(require 'ert)"
	$(BATCH) -l leaf-tests.el -f ert-run-tests-batch-and-exit

clean:
	-find . -type f -name "*.elc" | xargs rm
