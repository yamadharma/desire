;;; -*- mode: emacs-lisp; lexical-binding: t; coding: utf-8-unix; -*-
;;; ecf-mule/utf-8.el
;;; Force Emacs to default to UTF-8


;;; Activate UTF-8 mode
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(set-file-name-coding-system 'utf-8)
(set-clipboard-coding-system 'utf-8)
(setq default-process-coding-system '(utf-8 . utf-8))

;;; Backwards compatibility as default-buffer-file-coding-system is deprecated in 23.2.
(if (boundp 'buffer-file-coding-system)
    (setq-default buffer-file-coding-system 'utf-8)
  (setq default-buffer-file-coding-system 'utf-8))

;;; Treat clipboard input as UTF-8 string first; compound text next, etc.
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

(if IS-WINDOWS
    (set-w32-system-coding-system 'utf-8))

;;;
