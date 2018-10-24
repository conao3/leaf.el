EMACS := emacs
BATCH := $(EMACS) -Q --batch $(LOAD_PATH)
ELS   := leaf.el
ELCS  := $(ELS:.el=.elc)

all: build

build: $(ELCS)

%.elc: %.el
	@printf "Compiling $<\n"
	-@$(BATCH) -f batch-byte-compile $<

test:
	$(BATCH) --eval "(progn \
	(load-file \"leaf-tests.el\") \
	(ert-run-tests-batch-and-exit))"

clean:
	-find . -type f -name "*.elc" | xargs rm
