;; ecf-mule/ru.el -*- coding: iso-2022-7bit-unix; -*-

(if (< emacs-major-version 23)
(if 
    (assoc "866" (cp-supported-codepages))
    (codepage-setup 866)
)
nil
) ;; END if

(if (< emacs-major-version 23)
(if 
    (assoc "1251" (cp-supported-codepages))
    (progn
	(codepage-setup 1251)
	(define-coding-system-alias 'windows-1251 'cp1251)
	(define-coding-system-alias 'win-1251 'cp1251)
    ) ;; END progn
    nil	
) ;; END if
nil
) ;; END if

;;{{{ Create Cyrillic-CP1251 Language Environment menu item

(set-language-info-alist
  "Cyrillic-CP1251" 
  `(
     (charset cyrillic-iso8859-5)
     (coding-system cp1251)
     (coding-priority cp1251)
     (input-method . "cyrillic-jcuken")
     (features cyril-util)
     (unibyte-display . cp1251)
     (sample-text . "Russian (Русский)    Здравствуйте!")
     (documentation . "Support for Cyrillic CP1251.")
  )
  '("Cyrillic")
)

;;}}}
;;{{{

(if (>= emacs-major-version 22)
  (set-input-method 'russian-computer)
  (set-input-method 'cyrillic-jcuken)
)

;;}}}


(define-coding-system-alias 'koi8-u 'koi8-r)

;;

