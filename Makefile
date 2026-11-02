# desire — Makefile
# ---------------------------------------------------------------------------
# Targets:
#   make                  показать помощь
#   make sync-version     синхронизировать ;; Version: из package.json
#   make check-version    упасть, если заголовки не совпадают
#   make compile          байт-компилировать все .el
#   make checkdoc         прогнать checkdoc
#   make package-lint     прогнать package-lint (если установлен)
#   make test             прогнать ERT-тесты (если есть test/*.el)
#   make check            check-version + compile + checkdoc + package-lint + test
#   make release          финальные проверки + подсказка про коммит и тег
#   make clean            удалить .elc
#   make install-hooks    поставить .git/hooks/pre-commit
# ---------------------------------------------------------------------------

EMACS          ?= emacs
PYTHON         ?= python3

EL_FILES       := desire.el desire-straight.el desire-quelpa.el
ELC_FILES      := $(EL_FILES:.el=.elc)
TEST_DIR       ?= test
TEST_FILE      := $(TEST_DIR)/desire-test.el

PKG_JSON       := package.json
PY_SYNC        := scripts/sync_version.py
CHECKDOC_BATCH := scripts/checkdoc-batch.el

# Fail byte-compile on warnings?  Установите t в CI:
#   make compile STRICT_COMPILE=t
STRICT_COMPILE ?= nil

.DEFAULT_GOAL  := help

# ---------------------------------------------------------------------------
# Help
# ---------------------------------------------------------------------------
.PHONY: help
help:
	@echo "Targets:"
	@echo "  make sync-version    sync ';; Version:' headers from package.json"
	@echo "  make check-version   fail if headers are out of sync"
	@echo "  make compile         byte-compile all .el"
	@echo "  make checkdoc        run checkdoc"
	@echo "  make package-lint    run package-lint (skips if not installed)"
	@echo "  make test            run ERT tests in $(TEST_DIR)/"
	@echo "  make check           all of the above"
	@echo "  make release         checks + tag instructions"
	@echo "  make clean           remove *.elc"
	@echo "  make install-hooks   install .git/hooks/pre-commit"

# ---------------------------------------------------------------------------
# Version sync (Python)
# ---------------------------------------------------------------------------
.PHONY: sync-version check-version
sync-version:
	@$(PYTHON) $(PY_SYNC)
	@echo
	@echo "Review with: git diff $(EL_FILES)"

check-version:
	@$(PYTHON) $(PY_SYNC) --check || { \
	  echo ""; \
	  echo "Version headers are out of sync with $(PKG_JSON)."; \
	  echo "Run: make sync-version"; \
	  exit 1; \
	}

# ---------------------------------------------------------------------------
# Build
# ---------------------------------------------------------------------------
.PHONY: compile
compile:
	@$(EMACS) -Q --batch -L . \
	  --eval '(setq byte-compile-error-on-warn $(STRICT_COMPILE))' \
	  -f batch-byte-compile $(EL_FILES)

.PHONY: clean
clean:
	@rm -f $(ELC_FILES)
	@echo "clean: removed $(ELC_FILES)"

# ---------------------------------------------------------------------------
# Lint
# ---------------------------------------------------------------------------
.PHONY: checkdoc
checkdoc:
	@$(EMACS) -Q --batch -L . -l $(CHECKDOC_BATCH) -- $(EL_FILES)

.PHONY: package-lint
package-lint:
	@$(EMACS) -Q --batch -L . \
	  --eval "(unless (require 'package-lint nil t) \
	            (message \"package-lint not installed; skipping\") \
	            (kill-emacs 0))" \
	  --eval "(package-lint-batch-and-exit)" \
	  $(EL_FILES)

# ---------------------------------------------------------------------------
# Tests
# ---------------------------------------------------------------------------
.PHONY: test
test:
	@if [ -f $(TEST_FILE) ]; then \
	  $(EMACS) -Q --batch -L . -L $(TEST_DIR) \
	    -l ert -l $(TEST_FILE) \
	    -f ert-run-tests-batch-and-exit; \
	else \
	  echo "test: $(TEST_FILE) not found; skipping"; \
	fi

# ---------------------------------------------------------------------------
# Aggregate
# ---------------------------------------------------------------------------
.PHONY: check
check: check-version compile checkdoc package-lint test
	@echo
	@echo "All checks passed."

# ---------------------------------------------------------------------------
# Release helper (no git tag — только инструкция)
# ---------------------------------------------------------------------------
.PHONY: release
release: check-version compile test
	@echo
	@VER=$$($(PYTHON) -c 'import json,sys; print(json.load(open("$(PKG_JSON)"))["version"])'); \
	echo "Ready to release v$$VER."; \
	echo "Next steps:"; \
	echo "  git add $(PKG_JSON) $(EL_FILES)"; \
	echo "  git commit -m \"release: $$VER\""; \
	echo "  git tag -a v$$VER -m \"$$VER\""; \
	echo "  git push && git push --tags"

# ---------------------------------------------------------------------------
# Git hooks
# ---------------------------------------------------------------------------
.PHONY: install-hooks
install-hooks:
	@mkdir -p .git/hooks
	@printf '%s\n' \
	  '#!/bin/sh' \
	  'set -e' \
	  'make -s check-version' > .git/hooks/pre-commit
	@chmod +x .git/hooks/pre-commit
	@echo "installed .git/hooks/pre-commit"
