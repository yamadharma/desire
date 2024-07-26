;;; -*- coding: utf-8 -*-
;;; DejaVu fontset setup

;; For small screen users (eg. 640x480 or 800x600)
(create-fontset-from-fontset-spec
 "-misc-dejavu sans mono-medium-r-normal-*-12-*-*-*-*-*-fontset-dejavu12,
 -misc-dejavu sans mono-medium-r-normal-*-12-*-*-*-*-*-iso10646-1" 
 t
)

(create-fontset-from-fontset-spec
 "-misc-dejavu sans mono-medium-r-normal--14-*-*-*-*-*-fontset-dejavu14,
 -misc-dejavu sans mono-medium-r-normal--14-*-*-*-*-*-iso10646-1" 
 t
)

;; For meidum screen users (eg. 1024x780)

;; For meidum screen users (eg. 1024x780) suitable for Thai characters

;; For large screen users (eg. 1280x1024 or larger)


(set-default-font "fontset-dejavu12")

(setq default-frame-alist
  (append
    '(
      ;; By default, use 14 dots fonts	
      (font . "-misc-dejavu sans mono-medium-r-normal-*-12-*-*-*-*-*-fontset-dejavu12")
      (font . "-misc-dejavu sans mono-medium-r-normal-*-14-*-*-*-*-*-fontset-dejavu14")
    )
    default-frame-alist
  )
)

;;;

