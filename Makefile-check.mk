%.elc: %.el
	$(EMACS) -Q --batch -L `pwd` -f batch-byte-compile $<

check: $(ELS:.el=.elc)
	$(EMACS) -Q --batch -L `pwd` $(CORT_ARGS)

clean:
	find . -type f -name '*.elc' | xargs rm -rf
	find . -type fd -name '.make-*' | xargs rm -rf

clean-v:
	@$(call ECHO_RED-,'`make clean` will delete...','','\n')
	@find . -type f -name '*.elc' | xargs -n1
	@find . -type fd -name '.make-*' | xargs -n1

