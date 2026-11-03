;;; ecf-lang.el --- locale detection for desire  -*- lexical-binding: t; coding: utf-8-unix -*-

;;; Commentary:
;;
;; Exports two variables used by `rc.desire.el' (to build language
;; overlay directories) and by `ecf-mule.el' (to load per-locale
;; code pages):
;;
;;   ecf-lang       -- detected locale string, e.g. "ru_RU.UTF-8"
;;   ecf-lang-list  -- candidate config basenames, from most general
;;                     ("ru") to most specific ("ru_ru.utf-8").
;;
;; `ecf-lang-list' is consumed as follows:
;;
;;   rc.desire.el: (mapcar (lambda (x) (concat "lang/" x)) ecf-lang-list)
;;                 -> desire-lang-load-path, e.g. "lang/ru", "lang/utf-8"
;;
;;   ecf-mule.el:  (mapcar (lambda (x) (concat "ecf-mule/" x ".el"))
;;                         ecf-lang-list)
;;                 -> loads ecf-mule/ru.el, ecf-mule/utf-8.el, ...

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
