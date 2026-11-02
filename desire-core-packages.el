;;; -*- mode: emacs-lisp; lexical-binding: t; coding: utf-8-unix; -*-
;;; desire-core-packages.el --- optional straight.el bootstrap
;;; Commentary:
;; This file is *not* loaded automatically by `desire'.  Call
;; `desire-straight-ensure' (or pass `:pkgman 'straight' to `desire')
;; to bootstrap straight.el on demand.

;;; Code:

(defvar desire-straight-bootstrap-version 5
  "Version passed to straight.el's install.el.
Bump this when you want to force a re-download of install.el.")

(defun desire--straight-bootstrap-file ()
  "Return the path to straight.el's bootstrap.el."
  (expand-file-name "straight/repos/straight.el/bootstrap.el"
                    user-emacs-directory))

(defun desire-straight-ensure (&optional noerror)
  "Ensure straight.el is bootstrapped and loaded.
Return non-nil on success.  When NOERROR is non-nil, return nil instead
of signalling an error on network/install failure."
  (unless (fboundp 'straight-use-package)
    (let ((bootstrap-file (desire--straight-bootstrap-file))
          ;; Dynamic binding required by install.el:
          (bootstrap-version desire-straight-bootstrap-version))
      (unless (file-exists-p bootstrap-file)
        (let ((buf (condition-case err
                       (url-retrieve-synchronously
                        "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
                        'silent 'inhibit-cookies)
                     (error
                      (if noerror
                          nil
                        (signal (car err) (cdr err)))))))
          (unless (buffer-live-p buf)
            (if noerror
                (cl-return-from desire-straight-ensure nil)
              (error "Cannot download straight.el install.el")))
          (unwind-protect
              (with-current-buffer buf
                (goto-char (point-max))
                (eval-print-last-sexp))
            (kill-buffer buf))))
      (unless (file-exists-p bootstrap-file)
        (if noerror
            (cl-return-from desire-straight-ensure nil)
          (error "straight.el bootstrap file not found: %s"
                 bootstrap-file)))
      (load bootstrap-file nil 'nomessage)))
  t)

(provide 'desire-core-packages)
