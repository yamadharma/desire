;;; -*- mode: emacs-lisp; lexical-binding: t; coding: utf-8-unix; -*-
;;; desire-core-lib.el

;;; Macros

;;;
(cl-defmacro doplist! ((arglist plist &optional retval) &rest body)
  "Loop over a PLIST's (property value) pairs then return RETVAL.

Evaluate BODY with either ARGLIST bound to (cons PROP VAL) or, if ARGLIST is a
list, the pair is destructured into (CAR . CDR)."
  (declare (indent 1))
  (let ((plist-var (make-symbol "plist")))
    `(let ((,plist-var (copy-sequence ,plist)))
       (while ,plist-var
         (let ,(if (listp arglist)
                   `((,(pop arglist) (pop ,plist-var))
                     (,(pop arglist) (pop ,plist-var)))
                 `((,arglist (cons (pop ,plist-var)
                                   (pop ,plist-var)))))
           ,@body))
       ,retval)))

;;;
(defmacro plist-put! (plist &rest rest)
  "Set each PROP VALUE pair in REST to PLIST in-place."
  `(cl-loop for (prop value)
            on (list ,@rest) by #'cddr
            do ,(if (symbolp plist)
                    `(setq ,plist (plist-put ,plist prop value))
                  `(plist-put ,plist prop value))))

;;;
(defmacro plist-delete! (plist prop)
  "Delete PROP from PLIST in-place."
  `(setq ,plist (doom-plist-delete ,plist ,prop)))

(provide 'desire-core-lib)
;;;
