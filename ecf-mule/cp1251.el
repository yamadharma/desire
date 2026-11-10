;;; cp1251.el --- CP1251 (Windows Cyrillic) coding for desire  -*- lexical-binding: t; coding: utf-8-unix; -*-

;; Version:         1.4.0
;; URL:             https://github.com/yamadharma/desire
;; SPDX-License-Identifier: GPL-3.0-or-later

;; Copyright (C) 2002-2025 Dmitry S. Kulyabov

;; This file is NOT part of GNU Emacs.  Distributed under GPL-3.0-or-later,
;; same conditions as GNU Emacs.

;;; Commentary:
;;
;; Legacy CP1251 (Windows Cyrillic) setup.  On Emacs 23+ `windows-1251'
;; is built-in; this file only makes it the preferred coding system and
;; sets up the Cyrillic-CP1251 language environment.
;;
;; This file is a configuration snippet, not a library.

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
