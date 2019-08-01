emacs ?= emacs

cask:
	$(shell EMACS=$(emacs) cask --verbose --debug)
	$(shell EMACS=$(emacs) cask update --verbose --debug)

test:
	@echo "Using $(shell which $(emacs))..."
	EMACS=$(EMACS) cask exec buttercup -L .

clean:
	rm -f *.elc

.PHONY: cask test clean
