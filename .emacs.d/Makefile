# .emacs.d/Makefile -*-makefile-gmake-*-

# Based on John Wiegley's Makefile
# <https://github.com/jwiegley/dot-emacs/>

DIRS        = site-lisp themes elpa-25.2
SUBDIRS     = $(shell find $(DIRS) -maxdepth 1 ! -name .git -type d -print)
LIB_SOURCE  = $(wildcard site-lisp/*.el) $(wildcard themes/*.el)
TARGET      = $(patsubst %.el,%.elc, $(LIB_SOURCE)) \
              $(patsubst %.el,%.elc, init.el) #dot-gnus.el dot-org.el
EMACS       = emacs
EMACS_BATCH = $(EMACS) -Q -batch
LOADPATH    = -L . $(patsubst %,-L %, $(SUBDIRS))
BATCH_LOAD  = $(EMACS_BATCH) $(LOADPATH)

all: $(TARGET)

compile:
	for i in $(DIRS); do \
	    echo $$i \
	    $(BATCH_LOAD) --eval '(batch-byte-recompile-directory 0)' $$i; \
	done

%.elc: %.el
	@rm -f $@
	@echo Compiling file $<
	$(BATCH_LOAD) -f batch-byte-compile $<

init.elc: init.el init-local.elc custom.elc #dot-gnus.elc

clean:
	@rm -f *.elc
	@find . -name '*.elc' ! -name './elpa/*' | while read elcfile ; do \
	    elfile=$$(echo $$elcfile | sed 's/\.elc$$/.el/'); \
	    if ! test -f $$elfile; then \
	        echo Removing orphaned file: $$elcfile ; \
	        rm $$elcfile ; \
	    fi ; \
	    if test $$elfile -nt $$elcfile; then \
	        echo Removing outdated file: $$elcfile ; \
	        rm $$elcfile ; \
	    fi ; \
	done
