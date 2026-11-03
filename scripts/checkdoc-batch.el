;;; checkdoc-batch.el --- batch wrapper around checkdoc  -*- lexical-binding: t; coding: utf-8-unix; -*-

;;; Commentary:
;;
;; Usage:
;;   emacs -Q --batch -L . -l scripts/checkdoc-batch.el -- file1.el file2.el
;;
;; Exits with 0 if all files are clean, 1 if any checkdoc note was
;; produced.  Notes are printed to stderr via `message'.

;;; Code:

(require 'checkdoc)

(setq checkdoc-force-docstrings-flag nil
      checkdoc-spellcheck-documentation-flag nil)

(let ((failures 0))
  (dolist (file command-line-args-left)
    (with-current-buffer (find-file-noselect file)
      (let ((checkdoc-diagnostic-buffer "*Checkdoc*"))
        (when (get-buffer checkdoc-diagnostic-buffer)
          (with-current-buffer checkdoc-diagnostic-buffer
            (erase-buffer)))
        (checkdoc-current-buffer t)
        (when (get-buffer checkdoc-diagnostic-buffer)
          (with-current-buffer checkdoc-diagnostic-buffer
            (unless (= (buffer-size) 0)
              (message "%s:\n%s" file (buffer-string))
              (setq failures (1+ failures))))))))
  (if (> failures 0)
      (progn
        (message "checkdoc: %d file(s) with issues" failures)
        (kill-emacs 1))
    (message "checkdoc: OK")
    (kill-emacs 0)))

;;; checkdoc-batch.el ends here
