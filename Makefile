SHELL := /usr/bin/env bash

EMACS ?= emacs
CASK ?= cask

TEST-FILES := test/windows-bootstrap.el test/test-helper.el \
	$(shell ls test/lsp-ui-*.el)
LOAD-FILE = -l $(test-file)
LOAD-TEST-FILES := $(foreach test-file, $(TEST-FILES), $(LOAD-FILE))

build:
	EMACS=$(EMACS) cask install
	EMACS=$(EMACS) cask build
	EMACS=$(EMACS) cask clean-elc

unix-ci: build unix-compile clean

windows-ci: CASK=
windows-ci: windows-compile clean

unix-compile:
	@echo "Compiling..."
	@$(CASK) $(EMACS) -Q --batch \
		-L . \
		--eval '(setq byte-compile-error-on-warn t)' \
		-f batch-byte-compile \
		*.el

windows-compile:
	@echo "Compiling..."
	@$(CASK) $(EMACS) -Q --batch \
		-l test/windows-bootstrap.el \
		-L . \
		--eval '(setq byte-compile-error-on-warn t)' \
		-f batch-byte-compile \
		*.el

unix-test:
	EMACS=$(EMACS) cask exec ert-runner

windows-test:
	@$(EMACS) -Q --batch \
		-l test/windows-bootstrap.el \
		-L . \
		$(LOAD-TEST-FILES) \
		--eval "(ert-run-tests-batch-and-exit \
		'(and (not (tag no-win)) (not (tag org))))"

clean:
	rm -rf .cask *.elc

.PHONY: build ci unix-compile unix-test windows-compile windows-test clean
