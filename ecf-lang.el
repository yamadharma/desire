;;; ecf-lang.el --- locale detection for desire  -*- lexical-binding: t; coding: utf-8-unix; -*-

;; Version:         1.3.0
;; Package-Requires: ((emacs "24.3"))
;; Keywords:        convenience, i18n
;; URL:             https://github.com/yamadharma/desire
;; SPDX-License-Identifier: GPL-3.0-or-later

;; Copyright (C) 2002-2026 Dmitry S. Kulyabov

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
;; Locale detection for `desire'.
;;
;; Exports two variables:
;;
;;   `ecf-lang'       -- detected locale string, e.g. "ru_RU.UTF-8"
;;   `ecf-lang-list'  -- candidate config basenames, from most general
;;                       ("ru") to most specific ("ru_ru.utf-8")
;;
;; Used by:
;;   `rc.desire.el'  -- to build language overlay directories
;;   `ecf-mule.el'   -- to load per-locale code pages

;;; Code:

(defvar ecf-lang
  (or (and (eq system-type 'windows-nt)
           (fboundp 'w32-get-current-locale-id)
           (equal (w32-get-current-locale-id) 1049)
           "ru_RU.CP1251")
      (getenv "LANG")
      "C")
  "Detected locale string, e.g. \"ru_RU.UTF-8\".")

(defvar ecf-lang-list
  (let* ((lang       ecf-lang)
         (no-codeset (and (string-match "\\`\\([^.]+\\)" lang)
                          (match-string 1 lang)))
         (lang-only  (and (string-match "\\`\\([^_.]+\\)" lang)
                          (match-string 1 lang)))
         (codeset    (and (string-match "\\.\\([^@]+\\)" lang)
                          (match-string 1 lang)))
         (all        (list lang no-codeset lang-only codeset)))
    ;; "ru" (language) -> "ru_ru" (lang+region) ->
    ;; "utf-8" (codeset) -> "ru_ru.utf-8" (full locale)
    (delete-dups (delq nil (mapcar #'downcase all))))
  "Candidate config basenames, from most general to most specific.")

(provide 'ecf-lang)

;;; ecf-lang.el ends here
