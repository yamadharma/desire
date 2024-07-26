;; ecf-mule/koi8-r.el -*- coding: iso-2022-7bit-unix; -*-

;;{{{ Language environment

(set-language-environment "Cyrillic-KOI8")

;;}}}
;;{{{ Coding system

(set-default-coding-systems 'cyrillic-koi8)	
(prefer-coding-system 'cyrillic-koi8)

(setq default-buffer-file-coding-system 'cyrillic-koi8-unix)
(set-buffer-file-coding-system 'cyrillic-koi8-unix)

(set-terminal-coding-system 'cyrillic-koi8)
(set-keyboard-coding-system 'cyrillic-koi8)

;(setq-default coding-system-for-read 'cyrillic-koi8)
;(setq-default coding-system-for-write 'cyrillic-koi8)

(set-selection-coding-system 'cyrillic-koi8)
(setq selection-coding-system 'cyrillic-koi8)

(setq file-name-coding-system 'cyrillic-koi8)
;; (setq default-process-coding-system 'cyrillic-koi8)

(put-charset-property 'cyrillic-iso8859-5 'preferred-coding-system 'koi8-r)

;;}}}

