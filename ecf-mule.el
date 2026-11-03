;;; ecf-mule.el --- per-language configuration for desire  -*- lexical-binding: t; coding: utf-8-unix -*-

;;; Commentary:
;;
;; Loads language-specific configs from `ecf-mule/'.  For LANG=ru_RU.UTF-8
;; the loader tries, in order:
;;
;;   ecf-mule/ru.el           (language)
;;   ecf-mule/utf-8.el        (codeset)
;;   ecf-mule/ru_ru.el        (language + territory)
;;   ecf-mule/ru_ru.utf-8.el  (full locale)
;;
;; First match wins.  To layer multiple files per language, use the
;; per-file `desire' mechanism (`desire.ecf' / `desire.ecd/') rather
;; than an N-match loop.

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
