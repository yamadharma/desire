;;; desire-straight.el --- straight.el backend for desire  -*- lexical-binding: t; coding: utf-8-unix; -*-
;;; Commentary:
;;
;; Optional backend for `desire'.  Loaded lazily by
;; `desire--ensure-backend' when `:pkgman straight' is requested (or
;; when `(desire 'straight)' is called).  Replaces the side-effecting
;; `desire-core-packages.el'.
;;
;; Public API:
;;   (desire-straight-ensure &optional NOERROR)
;;      Bootstrap straight.el on demand and return non-nil on success.
;;   (desire-straight-use-package RECIPE)
;;      Install RECIPE via straight, bootstrapping first if needed.
;;
;;; Code:

(require 'cl-lib)
(require 'url)

(defvar desire-straight-bootstrap-version 5
  "Version passed to straight.el's install.el.
Bump when you want to force a re-download of install.el.")

(defvar desire-straight-install-url
  "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
  "URL of straight.el's install.el bootstrap script.")

(defun desire-straight--bootstrap-file ()
  "Return the path to straight.el's bootstrap.el."
  (expand-file-name "straight/repos/straight.el/bootstrap.el"
                    user-emacs-directory))

(defun desire-straight--download-install (&optional noerror)
  "Download install.el into a buffer and eval it.
Return the buffer on success, nil on failure when NOERROR is non-nil."
  (let ((buf (condition-case err
                 (url-retrieve-synchronously desire-straight-install-url
                                             'silent 'inhibit-cookies)
               (error
                (if noerror
                    nil
                  (signal (car err) (cdr err)))))))
    (unless (buffer-live-p buf)
      (if noerror
          (cl-return-from desire-straight--download-install nil)
        (error "Cannot download straight.el install.el from %s"
               desire-straight-install-url)))
    (unwind-protect
        (with-current-buffer buf
          (goto-char (point-max))
          (eval-print-last-sexp))
      (kill-buffer buf))
    buf))

(defun desire-straight-ensure (&optional noerror)
  "Ensure straight.el is bootstrapped and loaded.
Return non-nil on success.  When NOERROR is non-nil, return nil instead
of signalling on network/bootstrap failure."
  (unless (fboundp 'straight-use-package)
    (let ((bootstrap-file (desire-straight--bootstrap-file))
          ;; Dynamic binding required by install.el:
          (bootstrap-version desire-straight-bootstrap-version))
      (unless (file-exists-p bootstrap-file)
        (unless (desire-straight--download-install noerror)
          (cl-return-from desire-straight-ensure nil)))
      (unless (file-exists-p bootstrap-file)
        (if noerror
            (cl-return-from desire-straight-ensure nil)
          (error "straight.el bootstrap file not found: %s" bootstrap-file)))
      (load bootstrap-file nil 'nomessage)))
  t)

(defun desire-straight-use-package (recipe)
  "Install RECIPE with straight.el, bootstrapping it first.
RECIPE has the form (PACKAGE . PLIST)."
  (desire-straight-ensure)
  (straight-use-package recipe))

(provide 'desire-straight)

;;; desire-straight.el ends here
