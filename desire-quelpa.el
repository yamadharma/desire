;;; desire-quelpa.el --- quelpa backend for desire  -*- lexical-binding: t; coding: utf-8-unix; -*-

;; Version:         1.3.0
;; Package-Requires: ((emacs "24.3"))
;; Keywords: convenience
;; URL:             https://github.com/yamadharma/desire
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;;
;; Optional backend for `desire'.  Loaded lazily by
;; `desire--ensure-backend' when `:pkgman quelpa' is requested.
;; Quelpa is *not* available on ELPA/MELPA; we bootstrap it via
;; straight.el (preferred) or package-vc, in that order.
;;
;; Public API:
;;   (desire-quelpa-ensure &optional NOERROR)
;;      Bootstrap quelpa on demand and return non-nil on success.
;;   (desire-quelpa-use-package RECIPE)
;;      Install RECIPE via quelpa, bootstrapping first if needed.
;;
;;; Code:

(require 'cl-lib)

(defvar desire-quelpa-repository "https://github.com/quelpa/quelpa"
  "Source of quelpa used for bootstrapping when it is not installed.")

(defun desire-quelpa--ensure-via-straight ()
  "Try to bootstrap quelpa via straight.el.  Return non-nil on success."
  (when (require 'desire-straight nil t)
    (when (desire-straight-ensure :noerror)
      (condition-case _err
          (progn
            (straight-use-package
             (list 'quelpa :fetcher 'github :repo "quelpa/quelpa"))
            (require 'quelpa nil t))
        (error nil)))))

(defun desire-quelpa--ensure-via-vc ()
  "Try to bootstrap quelpa via `package-vc-install'.  Return non-nil on success."
  (when (fboundp 'package-vc-install)
    (condition-case _err
        (progn
          (package-vc-install desire-quelpa-repository)
          (require 'quelpa nil t))
      (error nil))))

(defun desire-quelpa-ensure (&optional noerror)
  "Ensure quelpa is available.
Tries straight.el first, then package-vc, then errors out (or returns
nil if NOERROR is non-nil)."
  (or (fboundp 'quelpa)
      (desire-quelpa--ensure-via-straight)
      (desire-quelpa--ensure-via-vc)
      (if noerror
          nil
        (error "Cannot bootstrap quelpa automatically; \
install it manually (e.g. via straight or package-vc), then re-run"))
      t))

(defun desire-quelpa-use-package (recipe)
  "Install RECIPE with quelpa, bootstrapping it first.
RECIPE has the form (PACKAGE . PLIST)."
  (desire-quelpa-ensure)
  (quelpa recipe))

(provide 'desire-quelpa)

;;; desire-quelpa.el ends here
