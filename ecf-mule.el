;;; ecf-mule.el --- per-language configuration for desire  -*- lexical-binding: t; coding: utf-8-unix; -*-

;; Version:         1.4.0
;; Package-Requires: ((emacs "24.3"))
;; Keywords:        convenience, i18n, mule
;; URL:             https://github.com/yamadharma/desire
;; SPDX-License-Identifier: GPL-3.0-or-later

;; Copyright (C) 2002-2026 Dmitry S. Kolyabov

;; This file is NOT part of GNU Emacs.  It is, however, distributed
;; under the same conditions as GNU Emacs, which are as follows:

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Loads the most specific language-specific config from `ecf-mule/'.
;; For `LANG=ru_RU.UTF-8' tries, in order:
;;
;;   ecf-mule/ru.el
;;   ecf-mule/utf-8.el
;;   ecf-mule/ru_ru.el
;;   ecf-mule/ru_ru.utf-8.el
;;
;; First existing file wins.  `load' is used (not `require') because
;; these locale files are configuration snippets, not libraries.
;;
;; Exports `desire-mule-lang', `desire-mule-candidates',
;; `desire-mule-load'.

;;; Code:

(require 'cl-lib)

(defconst desire-mule-dir "ecf-mule/"
  "Directory under `desire' with per-language configs.")

(defvar desire-mule-lang nil
  "Detected locale string, e.g. \"ru_RU.UTF-8\".")

(defvar desire-mule-candidates nil
  "Candidate config basenames, most specific first.")

(defun desire-mule--detect-lang ()
  "Return the current locale string, or \"C\" if unknown."
  (or (and (eq system-type 'windows-nt)
           (equal (w32-get-current-locale-id) 1049)
           "ru_RU.CP1251")
      (getenv "LANG")
      "C"))

(defun desire-mule--candidates (lang)
  "Return config basenames for LANG, most specific first."
  (let* ((no-codeset (and (string-match "\\`\\([^.]+\\)" lang)
                          (match-string 1 lang)))
         (lang-only  (and (string-match "\\`\\([^_.]+\\)" lang)
                          (match-string 1 lang)))
         (codeset    (and (string-match "\\.\\([^@]+\\)" lang)
                          (match-string 1 lang)))
         (all (list lang no-codeset lang-only codeset)))
    (delete-dups (delq nil (mapcar #'downcase all)))))

(defun desire-mule-load ()
  "Load the most specific `ecf-mule/' config for the current locale."
  (setq desire-mule-lang (desire-mule--detect-lang))
  (setq desire-mule-candidates (desire-mule--candidates desire-mule-lang))
  (let ((file (cl-find-if
               (lambda (name)
		 (locate-library (concat desire-mule-dir name)))
               desire-mule-candidates)))
    (if file
	(progn
          (message "ecf-mule: %s → %s" desire-mule-lang file)
          (load (concat desire-mule-dir file)))
      (message "ecf-mule: no config for %s" desire-mule-lang))))

(desire-mule-load)

(provide 'ecf-mule)

;;; ecf-mule.el ends here
