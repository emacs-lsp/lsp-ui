SHELL := /usr/bin/env bash

EMACS ?= emacs
EASK ?= eask

TEST-FILES := test/windows-bootstrap.el test/test-helper.el \
	$(shell ls test/lsp-ui-*.el)
LOAD-FILE = -l $(test-file)
LOAD-TEST-FILES := $(foreach test-file, $(TEST-FILES), $(LOAD-FILE))

# FIXME: add `test`, and `lint`
ci: clean build compile checkdoc

build:
	$(EASK) package
	$(EASK) install
	$(EASK) clean-elc

compile:
	@echo "Compiling..."
	@$(EASK) compile

test:
	@echo "Testing..."
	$(EASK) install-deps --dev
	$(EASK) exec ert-runner -L . $(LOAD-TEST-FILES) -t '!no-win' -t '!org'

checkdoc:
	@echo "Run checkdoc..."
	@$(EASK) lint checkdoc

lint:
	@echo "Run package-lint..."
	@$(EASK) lint package

clean:
	@$(EASK) clean-all

.PHONY: ci build compile test clean
