emacs ?= emacs
BATCH = --batch -Q

# annalist, ob-core, buttercup, etc. for indentation check
ELISP_LINT = -l elisp-lint -l buttercup -l ob-core
ELISP_LINT += --eval "(load-file \"annalist.el\")"
ELISP_LINT += --eval "(load-file \"tests/test-annalist.el\")"
ELISP_LINT += --eval "(load-file \"lint/lint-init.el\")"
ELISP_LINT += -f elisp-lint-files-batch

PACKAGE_LINT = -l package-lint -f package-lint-batch-and-exit

LOAD_OX = -l ox-extra # -l ox-texinfo+.el
EXPORT_TEXINFO = -f org-texinfo-export-to-texinfo

cask:
	EMACS=$(emacs) cask --verbose --debug
	EMACS=$(emacs) cask update --verbose --debug

test:
	@echo "Using $(shell which $(emacs))..."
	EMACS=$(emacs) cask exec buttercup -L .

package-lint:
	EMACS=$(emacs) cask emacs $(BATCH) $(PACKAGE_LINT) annalist.el

elisp-lint:
	EMACS=$(emacs) cask emacs $(BATCH) $(ELISP_LINT) annalist.el
	@# need to use absolute file path because of working directory issues
	@# need separate command or will fail trying to find annalist-autoloads.el
	EMACS=$(emacs) cask emacs $(BATCH) $(ELISP_LINT) \
		"$(shell realpath tests/test-annalist.el)"
	$(MAKE) clean

clean:
	rm -f *.elc tests/*.elc *-autoloads.el tests/*-autoloads.el *\~ tests/*\~

cask-texi:
	EMACS=$(emacs) cask --path texi --verbose --debug
	EMACS=$(emacs) cask update --path texi --verbose --debug

texi:
	$(MAKE) cask-texi
	@# put index version of readme in annalist.org
	@git show :README.org > texi/annalist.org
	@rm -f annalist.texi
	@# NOTE pre-commit hook will fail if wrap this line
	cd texi && EMACS=$(emacs) cask emacs $(BATCH) $(LOAD_OX) annalist.org $(EXPORT_TEXINFO)
	@mv texi/annalist.texi ./
	@# add missing final newline
	@echo >> annalist.texi
	@rm -f texi/annalist.org texi/*~

.PHONY: cask test package-lint elisp-lint clean cask-texi texi
