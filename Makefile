## Makefile

all:

include Makefunc.mk

TOP          := $(dir $(lastword $(MAKEFILE_LIST)))

UUID         := $(shell ((uuidgen > /dev/null 2>&1 && uuidgen) || echo $$) | cut -c -7)

UBUNTU_EMACS := 24.1 24.5 25.1
ALPINE_EMACS := 25.3 26.1 26.2
DOCKER_EMACS := $(UBUNTU_EMACS:%=ubuntu-min-%) $(ALPINE_EMACS:%=alpine-min-%)

DEPENDS      :=

EMACS        ?= emacs
BATCH        := $(EMACS) -Q --batch -L $(TOP) $(DEPENDS:%=-L ./%/)

TESTFILE     := leaf-tests.el
ELS          := leaf.el

CORTELS      := $(TESTFILE) cort-test.el

##################################################

.PHONY: all git-hook build check-22 check allcheck test clean-soft clean

all: git-hook build

##############################

git-hook:
	cp -a git-hooks/* .git/hooks/

build: $(ELS:%.el=%.elc)

%.elc: %.el $(DEPENDS)
	$(BATCH) $(DEPENDS:%=-L %/) -f batch-byte-compile $<

##############################
#
#  docker one-time test (on top level)
#

check: build
	$(BATCH) -l $(TESTFILE) -f cort-test-run

##############################
#
#  docker multi Emacs version test (on independent environment)
#

allcheck: $(DOCKER_EMACS:%=.make/verbose-${UUID}-emacs-test--%)
	@echo ""
	@cat $^ | grep =====
	@rm -rf $^

.make/verbose-%: .make $(DEPENDS)
	docker run -itd --name $* conao3/emacs:$(shell echo $* | sed "s/.*--//") /bin/sh
	docker cp . $*:/test
	docker exec $* sh -c "cd test && make clean-soft && make check -j" | tee $@
	docker rm -f $*

##############################
#
#  docker silent `allcheck' job
#

test: $(DOCKER_EMACS:%=.make/silent-${UUID}-emacs-test--%)
	@echo ""
	@cat $^ | grep =====
	@rm -rf $^

.make/silent-%: .make $(DEPENDS)
	docker run -itd --name $* conao3/emacs:$(shell echo $* | sed "s/.*--//") /bin/sh > /dev/null
	@docker cp . $*:/test
	@docker exec $* sh -c "cd test && make clean-soft && make check -j" > $@ || ( docker rm -f $*; cat $@ || false )
	@docker rm -f $* > /dev/null

.make:
	mkdir $@

##############################

clean-soft:
	rm -rf $(ELS:%.el=%.elc) .make

clean:
	rm -rf $(ELS:%.el=%.elc) $(DEPENDS) .make
