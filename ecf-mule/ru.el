;;; ru.el --- Russian language environment for desire  -*- lexical-binding: t; coding: utf-8-unix; -*-

;; Version:         1.3.0
;; URL:             https://github.com/yamadharma/desire
;; SPDX-License-Identifier: GPL-3.0-or-later

;; Copyright (C) 2002-2026 Dmitry S. Kulyabov

;; This file is NOT part of GNU Emacs.  Distributed under GPL-3.0-or-later,
;; same conditions as GNU Emacs.

;;; Commentary:
;;
;; Sets up Russian input method and language environment.  Assumes a
;; UTF-8 locale; for CP1251 see `cp1251.el'.
;;
;; This file is a configuration snippet, not a library: it is loaded
;; via `load' by `ecf-mule.el' and deliberately does not call
;; `(provide ...)'.

;;; Code:

(set-language-environment "Russian")

(setq default-input-method "russian-computer")

(set-language-info-alist
 "Russian"
 `((charset cyrillic-iso8859-5)
   (coding-system utf-8)
   (coding-priority utf-8)
   (input-method . "russian-computer")
   (sample-text . "Russian (Русский)     Приветствие!")
   (documentation . "Support for Russian."))
 '("Cyrillic"))

(provide 'ecf-mule/ru)

;;; ru.el ends here
