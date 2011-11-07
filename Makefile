#
# Makefile for emacs-revival
# 
# <hondana@gmx.com>
#

PACKAGE = emacs-revival

LS = ls
AWK = awk
EGREP = egrep
FIND=find
RM	= /bin/rm -f
CP	= /bin/cp -p

EMACS	= emacs
FLAGS   = -batch -q -no-site-file

LISPDIR =

# omit files with NO-BYTE-COMPILE tag to TRUE
SKIP_FILES := $(shell $(FIND) `pwd` -name "*.el" | xargs $(EGREP) '^ *;;.*+no-byte-compile: +t' | $(AWK) -F: '{ print $$1 }')
ELS := $(filter-out $(SKIP_FILES), $(wildcard $(PWD)/*.el $(PWD)/**/*.el))

do_path_template = (progn (cd \"$(path)\") (normal-top-level-add-subdirs-to-load-path))
SUBDIRS := $(shell $(FIND) `pwd` -type d | $(EGREP) -v "\/\.git")
DIRS = $(LISPDIR) $(SUBDIRS)
LOADPATHS := \
  $(patsubst %, -L %, $(DIRS)) \
  -eval "(progn $(foreach path, $(DIRS), $(do_path_template)))"

ELCS: $(ELS:.el=.elc)
		

all: $(ELCS)
	make $(ELCS)

.el.elc:
	$(EMACS) $(FLAGS) \
	   $(LOADPATHS) \
	   -f batch-byte-compile $*.el

clean:
	-$(RM) $(shell $(FIND) * -name "*.elc")

