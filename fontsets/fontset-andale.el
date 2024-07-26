;;; -*- coding: utf-8 -*-

(create-fontset-from-fontset-spec
  "-*-Andale Mono-normal-r-*-*-12-*-*-*-c-*-fontset-andale,
  latin-iso8859-2:-*-Andale Mono-normal-r-*-*-12-*-*-*-c-*-iso8859-2,
  latin-iso8859-3:-*-Andale Mono-normal-r-*-*-12-*-*-*-c-*-iso8859-3,
  latin-iso8859-4:-*-Andale Mono-normal-r-*-*-12-*-*-*-c-*-iso8859-4,
  cyrillic-iso8859-5:-*-Andale Mono-normal-r-*-*-12-*-*-*-c-*-iso8859-5,
  greek-iso8859-7:-*-Andale Mono-normal-r-*-*-12-*-*-*-c-*-iso8859-7,
  japanese-jisx0208:-*-MS Gothic-normal-r-*-*-12-*-*-*-c-*-jisx0208-sjis,
  katakana-jisx0201:-*-MS Gothic-normal-r-*-*-12-*-*-*-c-*-jisx0208-sjis,
  latin-jisx0201:-*-MS Gothic-normal-r-*-*-12-*-*-*-c-*-jisx0208-sjis,
  japanese-jisx0208-1978:-*-MS Gothic-normal-r-*-*-12-*-*-*-c-*-jisx0208-sjis"
)

(setq default-frame-alist
  (append
    '((font .
       "-*-Andale Mono-normal-r-*-*-12-*-*-*-c-*-fontset-andale"))
       default-frame-alist
  )
)

;;;