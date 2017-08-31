
# CS421 EWS cabal makefile
# 20170305 Eric Huber
# Updated to check that modules have been loaded.

default: checkmod nagmsg FORCE

bashloc := $(shell which bash)

SHELL = $(bashloc)

# commands for CBTF exam/ML

checkmod: FORCE
	@( \
	if [[ $${cs421_all_good} -ne 1 ]]; \
	then \
		echo "Module check FAILED!" ; \
		echo "You still need to source the cs421 loader file!" ; \
		false; \
	else \
		echo "Module check succeeded" ; \
		true; \
	fi )

build: checkmod FORCE
	-cabal sandbox init
	-cabal install --enable-tests --enable-relocatable
	-cabal build

run: checkmod FORCE
	-cabal sandbox init
	-cabal run

ghci: repl FORCE

repl: checkmod FORCE
	-cabal sandbox init
	-cabal repl

test: checkmod FORCE
	-cabal sandbox init
	-cabal test --show-details=always

clean: checkmod FORCE
	-cabal clean

# commands for EWS usage (non-CBTF)

ews-setup: checkmod FORCE
	-cabal sandbox init
	-cabal install --enable-tests --enable-relocatable
	-cabal build
	@echo "EWS setup complete"

ews-build: checkmod FORCE
	-cabal build

ews-ghci: ews-repl FORCE

ews-repl: checkmod FORCE
	-cabal repl

ews-run: checkmod FORCE
	-cabal run

ews-test: checkmod FORCE
	-cabal test

ews-clean: checkmod FORCE
	-cabal clean
	-rm -rf "$(CURDIR)/.cabal-sandbox"
	-rm -f "$(CURDIR)/cabal.sandbox.config"
	-rm -rf "$(CURDIR)/dist"
	@echo "WARNING: This command was for EWS only. If you just ran it"
	@echo "at the CBTF then you need to extract the tarball again."

# staff usage

nagmsg: FORCE
	@echo " "
	@echo "[IF YOU ARE AT CBTF] Do this to initialize the session:"
	@echo "  source cs421_loader.rc"
	@echo " Then you may use these as you like:"
	@echo "  make test"
	@echo "  make repl"
	@echo " "
	@echo "[IF YOU ARE ON EWS] If you are on EWS, NOT at the CBTF, do:"
	@echo "  source cs421_loader.rc"
	@echo "  make ews-setup"
	@echo " And then use these as you like:"
	@echo "  make ews-test"
	@echo "  make ews-repl"
	@echo " "

final_tarball = "$(abspath $(CURDIR)/../$(notdir $(CURDIR)).tar.bz2)"

dist: checkmod FORCE
	-cabal sandbox init
	-cabal install --only-dependencies --enable-tests --enable-relocatable
	-cabal build
	-cabal test --show-details=always
	-rm -rf "$(CURDIR)/.cabal-sandbox/logs"
	-rm -f "$(CURDIR)/cabal.sandbox.config"
	-rm -rf "$(CURDIR)/dist"
	-rm -f "$(CURDIR)/dist.tar.bz2"
	-rm -f "$(CURDIR)/$(notdir $(CURDIR)).tar.bz2"
	-rm -f $(final_tarball)
	-mkdir -p "${HOME}/tmp"
	-rm -f "${HOME}/tmp/$(notdir $(CURDIR)).tar.bz2"
	-tar -cjf "${HOME}/tmp/$(notdir $(CURDIR)).tar.bz2" "../$(notdir $(CURDIR))"
	-mv "${HOME}/tmp/$(notdir $(CURDIR)).tar.bz2" $(final_tarball)
	@echo "WARNING STUDENTS: This command was for instructors only."
	@echo "If you just ran it then you need to extract the tarball again."

sanity: FORCE
	@# @echo $(foo)
	@echo "$(notdir $(CURDIR))"
	@echo "$(CURDIR)/dist.tar.bz2"
	@echo "$(CURDIR)/$(notdir $(CURDIR)).tar.bz2"
	@echo $(final_tarball)

FORCE:

