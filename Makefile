# Root location of the Curry System specified by variable CURRYROOT
CURRYROOT := $(shell curry :set v0 :set -time :add Distribution :eval "putStrLn installDir" :quit)

# Curry system binary:
export CURRY = $(CURRYROOT)/bin/curry

# The tool name of the application:
TOOL = $(HOME)/.cpm/bin/cpm

# The default options for the REPL
export REPL_OPTS = --noreadline :set -time

# Source modules of CPM:
DEPS = src/CPM/*.curry src/CPM/*/*.curry

.PHONY: build
build: fetchdeps $(DEPS)
	@echo Root location of Curry system: $(CURRYROOT)
	@if [ ! -d "$(CURRYROOT)" ] ; then echo "Error: not a valid directory!" ; exit 1; fi
	@export CURRYPATH="";						\
	for i in `ls vendor`; do 					\
		export CURRYPATH="$$CURRYPATH:`pwd`/vendor/$$i/src"; 	\
	done; 								\
	echo "Set CURRYPATH to $$CURRYPATH"; 				\
	cd src; $(CURRY) $(REPL_OPTS) :l CPM.Main :save :quit
	mkdir -p $(dir $(TOOL))
	rm -f $(TOOL)
	cd $(dir $(TOOL)) && ln -s $(CURDIR)/src/CPM.Main $(notdir $(TOOL))
	@echo Tool installed into: $(TOOL)
	@echo Please add \"$(dir $(TOOL))\" to your path!

.PHONY: buildperf
buildperf: fetchdeps
	@export CURRYPATH="";						\
	for i in `ls vendor`; do 					\
		export CURRYPATH="$$CURRYPATH:`pwd`/vendor/$$i/src"; 	\
	done; 								\
	echo "Set CURRYPATH to $$CURRYPATH"; 				\
	cd src && $(CURRY) $(REPL_OPTS) :l CPM.PerformanceTest :save :quit

.PHONY: clean
clean:
	rm -Rf vendor
	rm -Rf src/.curry

.PHONY: fetchdeps
fetchdeps:
	./fetch-dependencies.sh

.PHONY: test
test: fetchdeps
	@export CURRYPATH="";						\
	for i in `ls vendor`; do					\
		export CURRYPATH="$$CURRYPATH:`pwd`/vendor/$$i/src";	\
	done;								\
	cd src; $(CURRY) check CPM.Package CPM.Resolution CPM.LookupSet

.PHONY: doc
doc: fetchdeps
	@export CURRYPATH="";						\
	for i in `ls vendor`; do					\
		export CURRYPATH="$$CURRYPATH:`pwd`/vendor/$$i/src";	\
	done;								\
	export CURRYPATH="$$CURRYPATH:`pwd`/src"; \
	$(CURRY) doc cdoc CPM.Main

.PHONY: cloc
cloc:
	cloc --force-lang=Haskell,curry --exclude-ext=hs  --exclude-lang=HTML,CSS \
		--not-match-f='PUBLIC' --by-file src/CPM

.PHONY: manual
manual:
	pdflatex -output-directory=docs docs/manual.tex
