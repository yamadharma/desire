;;; -*- coding: utf-8 -*-

(create-fontset-from-fontset-spec
 "-*-Courier New-normal-r-*-*-12-*-*-*-c-*-fontset-courier,
 latin-iso8859-2:-*-Courier New-normal-r-*-*-12-*-*-*-c-*-iso8859-2,
 latin-iso8859-3:-*-Courier New-normal-r-*-*-12-*-*-*-c-*-iso8859-3,
 latin-iso8859-4:-*-Courier New-normal-r-*-*-12-*-*-*-c-*-iso8859-4,
 cyrillic-iso8859-5:-*-Courier New-normal-r-*-*-12-*-*-*-c-*-iso8859-5,
 greek-iso8859-7:-*-Courier New-normal-r-*-*-12-*-*-*-c-*-iso8859-7,
 latin-iso8859-9:-*-Courier New-normal-r-*-*-12-*-*-*-c-*-iso8859-9,
 japanese-jisx0208:-*-MS Gothic-normal-r-*-*-12-*-*-*-c-*-jisx0208-sjis,
 katakana-jisx0201:-*-MS Gothic-normal-r-*-*-12-*-*-*-c-*-jisx0208-sjis,
 latin-jisx0201:-*-MS Gothic-normal-r-*-*-12-*-*-*-c-*-jisx0208-sjis,
 japanese-jisx0208-1978:-*-MS Gothic-normal-r-*-*-12-*-*-*-c-*-jisx0208-sjis,
 korean-ksc5601:-*-Gulim-normal-r-*-*-12-*-*-*-c-*-ksc5601-*,
 chinese-gb2312:-*-MS Song-normal-r-*-*-12-*-*-*-c-*-gb2312-*,
 chinese-big5-1:-*-MingLiU-normal-r-*-*-12-*-*-*-c-*-big5-*,
 chinese-big5-2:-*-MingLiU-normal-r-*-*-12-*-*-*-c-*-big5-*" t)

(set-default-font "fontset-courier")

(setq default-frame-alist
  (append
        '((font . 
          "-*-Courier New-normal-r-*-*-12-*-*-*-c-*-fontset-courier"))
          default-frame-alist
  )
)

;;;
