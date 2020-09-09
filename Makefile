SHELL := /usr/bin/env bash

EMACS ?= emacs
CASK ?= cask

build:
	EMACS=$(EMACS) cask install
	EMACS=$(EMACS) cask build
	EMACS=$(EMACS) cask clean-elc

ci: build compile clean
# TODO: Fix tests
# ci: build compile test clean

compile:
	@echo "Compiling..."
	@$(CASK) $(EMACS) -Q --batch \
		-L . \
		--eval '(setq byte-compile-error-on-warn t)' \
		-f batch-byte-compile \
		*.el

test:
	EMACS=$(EMACS) cask exec ert-runner

clean:
	EMACS=$(EMACS) cask clean-elc

.PHONY: build ci compile test clean
