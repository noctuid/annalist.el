emacs ?= emacs

cask:
	EMACS=$(emacs) cask --verbose --debug
	EMACS=$(emacs) cask update --verbose --debug

test:
	@echo "Using $(shell which $(emacs))..."
	EMACS=$(EMACS) cask exec buttercup -L .

clean:
	rm -f *.elc

.PHONY: cask test clean
