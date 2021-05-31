# Root location of the Curry System specified by variable CURRYROOT
CURRYROOT := $(shell curry --nocypm :set v0 :set -time :add Curry.Compiler.Distribution :eval "putStrLn installDir" :quit)

# Curry system binary:
export CURRY = $(CURRYROOT)/bin/curry

# The tool name of the application:
TOOL = $(HOME)/.cpm/bin/cypm

# The compiler name (e.g., pakcs or kics2):
CURRYCOMPILER := $(shell $(CURRY) --compiler-name)

# Executable of CurryCheck (for testing):
CURRYCHECK := $(shell which curry-check)

# The default options for the REPL (options "rts -T" required for KiCS2
# in order to get elapsed times):
ifeq ($(CURRYCOMPILER),kics2)
export REPL_OPTS = --noreadline :set -time :set rts -T
else
export REPL_OPTS = --nocypm --noreadline :set -time
endif

# Source modules of CPM:
DEPS = src/CPM/*.curry src/CPM/*/*.curry

.PHONY: build
build: fetchdeps src/CPM/ConfigPackage.curry $(DEPS)
	@echo Root location of Curry system: $(CURRYROOT)
	@if [ ! -d "$(CURRYROOT)" ] ; then echo "Error: not a valid directory!" ; exit 1; fi
	@export CURRYPATH="";						\
	for i in `ls vendor`; do 					\
		export CURRYPATH="$$CURRYPATH:`pwd`/vendor/$$i/src"; 	\
	done; 								\
	echo "Set CURRYPATH to $$CURRYPATH"; 				\
	cd src && $(CURRY) $(REPL_OPTS) :l CPM.Main :save :quit
	mkdir -p $(dir $(TOOL))
	rm -f $(TOOL)
	cd $(dir $(TOOL)) && ln -s $(CURDIR)/src/CPM.Main $(notdir $(TOOL))
	@echo Tool installed into: $(TOOL)
	@echo Please add \"$(dir $(TOOL))\" to your path!

src/CPM/ConfigPackage.curry: Makefile
	@echo "module CPM.ConfigPackage where" > $@
	@echo "packagePath :: String" >> $@
	@echo "packagePath = \"$(CURDIR)\"" >> $@
	@echo "packageVersion :: String" >> $@
	@echo "packageVersion = \"3.1.0\"" >> $@
	@echo "Curry configuration module '$@' written."

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
	if [ ! -x "$(CURRYCHECK)" ] ; then \
	  echo "Executable 'curry-check' is not installed!" && echo "To run the tests, install it by > cypm install currycheck" ; \
	else \
	  cd src && $(CURRYCHECK) CPM.Package CPM.Resolution CPM.LookupSet ; fi

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
