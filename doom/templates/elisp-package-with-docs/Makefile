CASK        ?= cask
EMACS       ?= emacs
DIST        ?= dist
EMACSFLAGS   = --batch -Q
EMACSBATCH   = $(EMACS) $(EMACSFLAGS)

VERSION     := $(shell EMACS=$(EMACS) $(CASK) version)
PKG_DIR     := $(shell EMACS=$(EMACS) $(CASK) package-directory)
PROJ_ROOT   := $(shell dirname $(realpath $(lastword $(MAKEFILE_LIST))))

EMACS_D      = ~/.emacs.d
USER_ELPA_D  = $(EMACS_D)/elpa

SRCS         = $(filter-out %-pkg.el, $(wildcard *.el))
TESTS        = $(wildcard test/*.el)
ORG_MANUAL   = doc/__PROJECT-NAME__.org
TEXI_MANUAL  = doc/__PROJECT-NAME__.texi
TAR          = $(DIST)/__PROJECT-NAME__-$(VERSION).tar


.PHONY: all check test unit install uninstall reinstall clean-all clean clean-elc

all : $(PKG_DIR) $(TAR)

install : $(TAR)
	$(EMACSBATCH) -l package -f package-initialize \
	--eval '(add-to-list (quote package-archives) (cons "melpa" "https://melpa.org/packages/") t)' \
	--eval '(package-install-file "$(PROJ_ROOT)/$(TAR)")'

uninstall :
	rm -rf $(USER_ELPA_D)/__PROJECT-NAME__-*

reinstall : clean uninstall install

clean-all : clean
	rm -rf $(PKG_DIR)

clean-elc :
	rm -f *.elc

clean : clean-elc
	rm -rf $(DIST)
	rm -f $(TEXI_MANUAL)
	rm -f *-pkg.el

$(PKG_DIR) : Cask
	$(CASK) install
	touch $(PKG_DIR)

$(TAR) : $(DIST) $(TEXI_MANUAL)
	$(CASK) package

$(DIST) :
	mkdir $(DIST)

$(TEXI_MANUAL) : $(PKG_DIR) $(ORG_MANUAL)
	$(CASK) exec $(EMACSBATCH) \
	-l org -l ox-texinfo \
	--file=$(ORG_MANUAL) -f org-texinfo-export-to-texinfo

check : test lint

test: unit

unit: $(PKG_DIR)
	${CASK} exec ert-runner
