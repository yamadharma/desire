;;; utf-8.el --- UTF-8 coding for desire  -*- lexical-binding: t; coding: utf-8-unix; -*-

;; Version:         1.4.0
;; URL:             https://github.com/yamadharma/desire
;; SPDX-License-Identifier: GPL-3.0-or-later

;; Copyright (C) 2002-2025 Dmitry S. Kulyabov

;; This file is NOT part of GNU Emacs.  Distributed under GPL-3.0-or-later,
;; same conditions as GNU Emacs.

;;; Commentary:
;;
;; Makes UTF-8 the preferred coding system for buffers, files,
;; keyboard, terminal, selections, and subprocesses.  This is the
;; modern default on Linux/macOS; the file exists to make it explicit
;; and to override distro defaults on Windows/Cygwin.
;;
;; This file is a configuration snippet, not a library.

;;; Code:

(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(set-file-name-coding-system 'utf-8)
(setq default-process-coding-system '(utf-8-unix . utf-8-unix))

(setq-default buffer-file-coding-system 'utf-8-unix)

;; Clipboard: prefer UTF-8 text, fall back to compound-text.
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

(when (fboundp 'set-w32-system-coding-system)
  (set-w32-system-coding-system 'utf-8))

(provide 'ecf-mule/utf-8)

;;; utf-8.el ends here
