;;; ecf-mule.el -*- coding: iso-2022-7bit-unix; -*-

(require 'ecf-lang)

(let
    ((dot-cands-list ecf-lang-list))
  
  (setq dot-cands-list
	(mapcar (function (lambda (x) (concat system-dot-emacs x ".el"))) dot-cands-list))
  
  (while (consp dot-cands-list)
    (if (locate-library (car dot-cands-list))
	(prog1
	    (load (car dot-cands-list))
	  (setq dot-cands-list
		(cdr dot-cands-list))
	  ;; (setq dot-cands-list nil)
	  )
      (setq dot-cands-list
	    (cdr dot-cands-list))
      )
    )
  ) ;; let ends here

(provide 'ecf-mule)

;;; ecf-mule.el ends here
