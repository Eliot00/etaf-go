EMACS ?= $(shell command -v emacs 2>/dev/null || echo /Applications/Emacs.app/Contents/MacOS/Emacs)
PACKAGE_DIR ?= $(HOME)/.emacs.d/elpa
LOAD = -L . -L test
INIT = --eval "(progn (require 'package) (setq package-user-dir \"$(PACKAGE_DIR)\") (package-initialize))"
MODULES = etaf-go-rules.el etaf-go-sgf.el etaf-go-view.el

.PHONY: compile test check smoke lint clean

compile: $(MODULES) etaf-go.el
	$(EMACS) -Q --batch $(LOAD) $(INIT) \
	  -f batch-byte-compile $(MODULES) etaf-go.el

test:
	$(EMACS) -Q --batch $(LOAD) $(INIT) \
	  -l test/etaf-go-test.el -f ert-run-tests-batch-and-exit

check: compile test

smoke:
	$(EMACS) -Q --batch $(LOAD) $(INIT) \
	  --eval "(progn (require 'etaf-go) (let ((b (etaf-go-demo))) (message \"demo: %d bytes\" (buffer-size b)) (etaf-go-close b)))"

lint:
	@if command -v eldev >/dev/null 2>&1; then eldev lint; \
	else echo "Eldev not installed; run package-lint/checkdoc yourself."; fi

clean:
	rm -f *.elc test/*.elc
