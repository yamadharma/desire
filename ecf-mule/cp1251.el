;; ecf-mule/cp1251.el -*- coding: iso-2022-7bit-unix; -*-

;;{{{ Language environment

(set-language-environment "Cyrillic-CP1251")

;;}}}
;;{{{ Coding system

(define-coding-system-alias 'windows-1251 'cp1251-dos)
(set-w32-system-coding-system 'cp1251)

(set-default-coding-systems 'cp1251)
(prefer-coding-system 'cp1251)
(setq default-buffer-file-coding-system 'cp1251-unix)
(set-buffer-file-coding-system 'cp1251-unix)
(set-keyboard-coding-system 'cp1251)
(set-terminal-coding-system 'cp1251)
(set-keyboard-coding-system 'cp1251)
(setq file-name-coding-system 'cp1251)
(set-selection-coding-system 'cp1251)

;;}}}
