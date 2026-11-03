;;; cp1251.el --- CP1251 (Windows Cyrillic) coding for desire  -*- lexical-binding: t; coding: utf-8-unix -*-

;;; Commentary:
;; Legacy file.  On Emacs 23+ `windows-1251' is built-in; this file
;; only makes it the preferred coding system.

;;; Code:

(set-language-environment "Cyrillic-CP1251")
(prefer-coding-system 'windows-1251)
(set-selection-coding-system 'windows-1251)
(set-file-name-coding-system 'windows-1251)

(setq-default buffer-file-coding-system 'windows-1251-unix)

(when (fboundp 'set-w32-system-coding-system)
  (set-w32-system-coding-system 'windows-1251))

(provide 'ecf-mule/cp1251)

;;; cp1251.el ends here
