## Makefile

# This program is free software: you can redistribute it and/or modify
# it under the terms of the Affero GNU General Public License as
# published by the Free Software Foundation, either version 3 of the
# License, or (at your option) any later version.

# This program is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the Affero
# GNU General Public License for more details.

# You should have received a copy of the Affero GNU General Public
# License along with this program.  If not, see
# <https://www.gnu.org/licenses/>.

all:

include Makefunc.mk

TOP          := $(dir $(lastword $(MAKEFILE_LIST)))
EMACS_RAW    := $(sort $(shell compgen -c emacs- | xargs))
EXPECT_EMACS := 22.1 21.2 21.3 21.4
EXPECT_EMACS  += 23.1 23.2 23.4
EXPECT_EMACS  += 24.1 24.2 24.3 24.4 24.5
EXPECT_EMACS  += 25.1 25.2 25.3
EXPECT_EMACS  += 26.1 26.2

ALL_EMACS    := $(filter $(EMACS_RAW),$(EXPECT_EMACS:%=emacs-%))

EMACS        ?= emacs
BATCH        := $(EMACS) -Q --batch -L $(TOP)

DEPENDS      :=

TESTFILE     := leaf-tests.el
ELS          := leaf.el

CORTELS      := $(TESTFILE) cort-test.el

##################################################

.PHONY: all git-hook build check allcheck test clean clean-v

all: git-hook build

##############################

git-hook:
	cp -a git-hooks/* .git/hooks/

build: $(ELS:%.el=%.elc)

%.elc: %.el $(DEPENDS)
	$(BATCH) $(DEPENDS:%=-L %/) -f batch-byte-compile $<

##############################
#
#  one-time test (on top level)
#

check: build
	$(BATCH) -l $(TESTFILE) -f cort-test-run

##############################
#
#  multi Emacs version test (on independent environment)
#

allcheck: $(ALL_EMACS:%=.make/verbose-%)
	@echo ""
	@cat $(^:%=%/.make-test-log) | grep =====
	@rm -rf $^

.make/verbose-%: $(DEPENDS)
	mkdir -p $@
	cp -rf $(ELS) $(CORTELS) $(DEPENDS) $@/
	cd $@; echo $(ELS) | xargs -n1 -t $* -Q --batch -L ./ $(DEPENDS:%=-L ./%/) -f batch-byte-compile
	cd $@; $* -Q --batch -L ./ -l $(TESTFILE) -f cort-test-run | tee .make-test-log

##############################
#
#  silent `allcheck' job
#

test: $(ALL_EMACS:%=.make/silent-%)
	@echo ""
	@cat $(^:%=%/.make-test-log) | grep =====
	@rm -rf $^

.make/silent-%: $(DEPENDS)
	@mkdir -p $@
	@cp -rf $(ELS) $(CORTELS) $(DEPENDS) $@/
	@cd $@; echo $(ELS) | xargs -n1 $* -Q --batch -L ./ $(DEPENDS:%=-L ./%/) -f batch-byte-compile
	@cd $@; $* -Q --batch -L ./ -l $(TESTFILE) -f cort-test-run > .make-test-log 2>&1

##############################

clean:
	rm -rf $(ELC) $(DEPENDS) .make
