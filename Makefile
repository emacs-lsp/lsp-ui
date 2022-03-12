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

# FIXME: Add `unix-test` and `windows-test`
ci: build compile clean

compile:
	@echo "Compiling..."
	@$(CASK) $(EMACS) -Q --batch \
		-L . \
		--eval '(setq byte-compile-error-on-warn t)' \
		-f batch-byte-compile \
		*.el

unix-test:
	EMACS=$(EMACS) cask exec ert-runner

windows-test:
	@$(EMACS) -Q --batch \
		--eval '(setq emacs-lsp-ci t)' \
		-l test/windows-bootstrap.el \
		-L . \
		$(LOAD-TEST-FILES) \
		--eval "(ert-run-tests-batch-and-exit \
		'(and (not (tag no-win)) (not (tag org))))"

clean:
	rm -rf .cask *.elc

.PHONY: build ci compile unix-test windows-test clean
