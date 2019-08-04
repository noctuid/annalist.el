emacs ?= emacs
BATCH = --batch -Q
LOAD = -l ox-extra # -l ox-texinfo+.el
EXPORT_TEXINFO = -f org-texinfo-export-to-texinfo

cask:
	EMACS=$(emacs) cask --verbose --debug
	EMACS=$(emacs) cask update --verbose --debug

test:
	@echo "Using $(shell which $(emacs))..."
	EMACS=$(emacs) cask exec buttercup -L .

clean:
	rm -f *.elc

texi:
	@# put index version of readme in annalist.org
	@git show :README.org > annalist.org
	@rm -f annalist.texi
	EMACS=$(emacs) cask emacs $(BATCH) $(LOAD) annalist.org $(EXPORT_TEXINFO)
	@# Add missing final newline
	@echo >> annalist.texi

.PHONY: cask test clean texi
