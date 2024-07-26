;;; -*- coding: utf-8 -*-
;;; intlfonts setup

;; For small screen users (eg. 640x480 or 800x600)
;(create-fontset-from-fontset-spec
; "-etl-*-medium-r-normal-*-14-*-*-*-*-*-fontset-14,
; latin-iso8859-1:-etl-*-medium-r-normal-*-14-*-*-*-*-*-iso8859-1,
; latin-iso8859-2:-etl-*-medium-r-normal-*-14-*-*-*-*-*-iso8859-2,
; latin-iso8859-3:-etl-*-medium-r-normal-*-14-*-*-*-*-*-iso8859-3,
; latin-iso8859-4:-etl-*-medium-r-normal-*-14-*-*-*-*-*-iso8859-4,
; cyrillic-iso8859-5:-etl-*-medium-r-normal-*-14-*-*-*-*-*-iso8859-5,
; greek-iso8859-7:-etl-*-medium-r-normal-*-14-*-*-*-*-*-iso8859-7,
; latin-iso8859-9:-etl-*-medium-r-normal-*-14-*-*-*-*-*-iso8859-9,
; japanese-jisx0208:-*-MS Gothic-normal-r-*-*-12-*-*-*-c-*-jisx0208-sjis,
; katakana-jisx0201:-*-MS Gothic-normal-r-*-*-12-*-*-*-c-*-jisx0208-sjis,
; latin-jisx0201:-*-MS Gothic-normal-r-*-*-12-*-*-*-c-*-jisx0208-sjis,
; japanese-jisx0208-1978:-*-MS Gothic-normal-r-*-*-12-*-*-*-c-*-jisx0208-sjis,
; japanese-jisx0208-1983:-misc-*-medium-r-normal-*-16-*-*-*-*-*-jisx0208.1983-0,
; korean-ksc5601:-*-Gulim-normal-r-*-*-12-*-*-*-c-*-ksc5601-*,
; chinese-gb2312:-*-MS Song-normal-r-*-*-12-*-*-*-c-*-gb2312-*,
; chinese-big5-1:-*-MingLiU-normal-r-*-*-12-*-*-*-c-*-big5-*,
; chinese-big5-2:-*-MingLiU-normal-r-*-*-12-*-*-*-c-*-big5-*" 
; t
;)


;; For small screen users (eg. 640x480 or 800x600)
(create-fontset-from-fontset-spec
 "-etl-*-medium-r-normal-*-14-*-*-*-*-*-fontset-intlfonts14,
 latin-iso8859-1:-etl-*-medium-r-normal-*-14-*-*-*-*-*-iso8859-1,
 latin-iso8859-2:-etl-*-medium-r-normal-*-14-*-*-*-*-*-iso8859-2,
 latin-iso8859-3:-etl-*-medium-r-normal-*-14-*-*-*-*-*-iso8859-3,
 latin-iso8859-4:-etl-*-medium-r-normal-*-14-*-*-*-*-*-iso8859-4,
 cyrillic-iso8859-5:-etl-*-medium-r-normal-*-14-*-*-*-*-*-iso8859-5,
 greek-iso8859-7:-etl-*-medium-r-normal-*-14-*-*-*-*-*-iso8859-7,
 latin-iso8859-9:-etl-*-medium-r-normal-*-14-*-*-*-*-*-iso8859-9,
 japanese-jisx0208:-*-MS Gothic-normal-r-*-*-12-*-*-*-c-*-jisx0208-sjis,
 katakana-jisx0201:-*-MS Gothic-normal-r-*-*-12-*-*-*-c-*-jisx0208-sjis,
 latin-jisx0201:-*-MS Gothic-normal-r-*-*-12-*-*-*-c-*-jisx0208-sjis,
 japanese-jisx0208-1978:-*-MS Gothic-normal-r-*-*-12-*-*-*-c-*-jisx0208-sjis,
 japanese-jisx0208-1983:-misc-*-medium-r-normal-*-16-*-*-*-*-*-jisx0208.1983-0,
 korean-ksc5601:-*-Gulim-normal-r-*-*-12-*-*-*-c-*-ksc5601-*,
 chinese-gb2312:-*-MS Song-normal-r-*-*-12-*-*-*-c-*-gb2312-*,
 chinese-big5-1:-*-MingLiU-normal-r-*-*-12-*-*-*-c-*-big5-*,
 chinese-big5-2:-*-MingLiU-normal-r-*-*-12-*-*-*-c-*-big5-*" 
 t
)

;; For meidum screen users (eg. 1024x780)
(create-fontset-from-fontset-spec
 "-etl-*-medium-r-normal-*-16-*-*-*-*-*-fontset-intlfonts16,
 latin-iso8859-1:-etl-*-medium-r-normal-*-16-*-*-*-*-*-iso8859-1,
 latin-iso8859-2:-etl-*-medium-r-normal-*-16-*-*-*-*-*-iso8859-2,
 latin-iso8859-3:-etl-*-medium-r-normal-*-16-*-*-*-*-*-iso8859-3,
 latin-iso8859-4:-etl-*-medium-r-normal-*-16-*-*-*-*-*-iso8859-4,
 cyrillic-iso8859-5:-etl-*-medium-r-normal-*-16-*-*-*-*-*-iso8859-5,
 greek-iso8859-7:-etl-*-medium-r-normal-*-16-*-*-*-*-*-iso8859-7,
 latin-iso8859-9:-etl-*-medium-r-normal-*-16-*-*-*-*-*-iso8859-9,
 japanese-jisx0208:-*-MS Gothic-normal-r-*-*-12-*-*-*-c-*-jisx0208-sjis,
 katakana-jisx0201:-*-MS Gothic-normal-r-*-*-12-*-*-*-c-*-jisx0208-sjis,
 latin-jisx0201:-*-MS Gothic-normal-r-*-*-12-*-*-*-c-*-jisx0208-sjis,
 japanese-jisx0208-1978:-*-MS Gothic-normal-r-*-*-12-*-*-*-c-*-jisx0208-sjis,
 japanese-jisx0208-1983:-jis-*-medium-r-normal-*-16-*-*-*-*-*-jisx0208.1983-0,
 korean-ksc5601:-*-Gulim-normal-r-*-*-12-*-*-*-c-*-ksc5601-*,
 chinese-gb2312:-*-MS Song-normal-r-*-*-12-*-*-*-c-*-gb2312-*,
 chinese-big5-1:-*-MingLiU-normal-r-*-*-12-*-*-*-c-*-big5-*,
 chinese-big5-2:-*-MingLiU-normal-r-*-*-12-*-*-*-c-*-big5-*" 
 t
)

;; For meidum screen users (eg. 1024x780) suitable for Thai characters
(create-fontset-from-fontset-spec
 "-etl-*-medium-r-normal-*-18-*-*-*-*-*-fontset-intlfonts18,
 latin-iso8859-1:-etl-*-medium-r-normal-*-18-*-*-*-*-*-iso8859-1,
 latin-iso8859-2:-etl-*-medium-r-normal-*-18-*-*-*-*-*-iso8859-2,
 latin-iso8859-3:-etl-*-medium-r-normal-*-18-*-*-*-*-*-iso8859-3,
 latin-iso8859-4:-etl-*-medium-r-normal-*-18-*-*-*-*-*-iso8859-4,
 cyrillic-iso8859-5:-etl-*-medium-r-normal-*-18-*-*-*-*-*-iso8859-5,
 greek-iso8859-7:-etl-*-medium-r-normal-*-18-*-*-*-*-*-iso8859-7,
 latin-iso8859-9:-etl-*-medium-r-normal-*-18-*-*-*-*-*-iso8859-9,
 japanese-jisx0208:-*-MS Gothic-normal-r-*-*-12-*-*-*-c-*-jisx0208-sjis,
 katakana-jisx0201:-*-MS Gothic-normal-r-*-*-12-*-*-*-c-*-jisx0208-sjis,
 latin-jisx0201:-*-MS Gothic-normal-r-*-*-12-*-*-*-c-*-jisx0208-sjis,
 japanese-jisx0208-1978:-*-MS Gothic-normal-r-*-*-12-*-*-*-c-*-jisx0208-sjis,
 japanese-jisx0208-1983:-jis-*-medium-r-normal-*-18-*-*-*-*-*-jisx0208.1983-0,
 korean-ksc5601:-*-Gulim-normal-r-*-*-12-*-*-*-c-*-ksc5601-*,
 chinese-gb2312:-*-MS Song-normal-r-*-*-12-*-*-*-c-*-gb2312-*,
 chinese-big5-1:-*-MingLiU-normal-r-*-*-12-*-*-*-c-*-big5-*,
 chinese-big5-2:-*-MingLiU-normal-r-*-*-12-*-*-*-c-*-big5-*" 
 t
)

;; For large screen users (eg. 1280x1024 or larger)
(create-fontset-from-fontset-spec
 "-etl-*-medium-r-normal-*-24-*-*-*-*-*-fontset-intlfonts24,
 latin-iso8859-1:-etl-*-medium-r-normal-*-24-*-*-*-*-*-iso8859-1,
 latin-iso8859-2:-etl-*-medium-r-normal-*-24-*-*-*-*-*-iso8859-2,
 latin-iso8859-3:-etl-*-medium-r-normal-*-24-*-*-*-*-*-iso8859-3,
 latin-iso8859-4:-etl-*-medium-r-normal-*-24-*-*-*-*-*-iso8859-4,
 cyrillic-iso8859-5:-etl-*-medium-r-normal-*-24-*-*-*-*-*-iso8859-5,
 greek-iso8859-7:-etl-*-medium-r-normal-*-24-*-*-*-*-*-iso8859-7,
 latin-iso8859-9:-etl-*-medium-r-normal-*-24-*-*-*-*-*-iso8859-9,
 japanese-jisx0208:-*-MS Gothic-normal-r-*-*-12-*-*-*-c-*-jisx0208-sjis,
 katakana-jisx0201:-*-MS Gothic-normal-r-*-*-12-*-*-*-c-*-jisx0208-sjis,
 latin-jisx0201:-*-MS Gothic-normal-r-*-*-12-*-*-*-c-*-jisx0208-sjis,
 japanese-jisx0208-1978:-*-MS Gothic-normal-r-*-*-12-*-*-*-c-*-jisx0208-sjis,
 japanese-jisx0208-1983:-jis-*-medium-r-normal-*-24-*-*-*-*-*-jisx0208.1983-0,
 korean-ksc5601:-*-Gulim-normal-r-*-*-12-*-*-*-c-*-ksc5601-*,
 chinese-gb2312:-*-MS Song-normal-r-*-*-12-*-*-*-c-*-gb2312-*,
 chinese-big5-1:-*-MingLiU-normal-r-*-*-12-*-*-*-c-*-big5-*,
 chinese-big5-2:-*-MingLiU-normal-r-*-*-12-*-*-*-c-*-big5-*" 
 t
)

(setq default-frame-alist
  (append
    '(
      ;; By default, use 14 dots fonts	
      (font . "-etl-*-medium-r-normal-*-14-*-*-*-*-*-fontset-intlfonts14")
      (font . "-etl-*-medium-r-normal-*-16-*-*-*-*-*-fontset-intlfonts16")
      (font . "-etl-*-medium-r-normal-*-18-*-*-*-*-*-fontset-intlfonts18")
      (font . "-etl-*-medium-r-normal-*-24-*-*-*-*-*-fontset-intlfonts24")
    )
    default-frame-alist
  )
)

;;;

