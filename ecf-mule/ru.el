;;; ru.el --- Russian language environment for desire  -*- lexical-binding: t; coding: utf-8-unix -*-

;;; Commentary:
;; Sets up Russian input and language environment.  Assumes UTF-8
;; locale; for CP1251 see cp1251.el.

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
