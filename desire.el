;;; -*- mode: emacs-lisp; lexical-binding: t; coding: utf-8-unix; -*-
;;; desire.el --- versatile configuration for emacs lisp packages

;; Authors:         Martin Schwenke <martin@meltin.net>
;;                  Graham Williams <Graham.Williams@cmis.csiro.au>
;;		    Dmitry S. Kulyabov <yamadharma@gmail.com>
;; Maintainer:      Dmitry S. Kulyabov <yamadharma@gmail.com>
;; Created:         20-Jun-1995

;; Keywords: setup configuration

;; Copyright (C) 1995-2001 Martin Schwenke and Graham Williams
;; Copyright (C) 2002-2025 Dmitry S. Kulyabov

;; This file is NOT part of GNU Emacs.  It is, however, distributed
;; under the same conditions as GNU Emacs, which are as follows:

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

(require 'cl-lib)
(require 'desire-core-lib)
(require 'desire-core-packages)

(defvar desire-load-path nil
  "*List of directories to be searched by `desire' for configuration data.")

(defvar desire-extension ".ecf"
  "*The extension given to configuration files used by `desire'.")

(defvar desire-dir-extension ".ecd"
  "*The extension given to configuration directories used by `desire'.")

(defvar desire-loaddefs "loaddefs"
  "*A string which is the name of a configuration file that is loaded
before a package itself is loaded.  This is useful for autoloading
functions.")

(defvar desire-desire "desire"
  "*A string which is the name of a configuration file that is loaded
after a package itself is loaded, but before the configuration files
for any other desirable package.  This is useful for performing some
initial setup.")

(defvar desire-execute "execute"
  "*A string which is the name of a configuration file that is loaded
after a package itself is loaded, and also after the configuration
files for any other desirable package.  This is useful for performing
some final setup.")

(defvar desire-postinstall "postinstall"
  "*A string which is the name of a configuration file that is loaded
after a package itself is installed. This is useful for performing
some final setup.")

(defvar desire-package-autoinstall t
  "*Enable automatic installation of a package if it is not present on the system.")

(defvar desirable nil
  "A list of strings for packages which have been successfully desired.
The function `desired' will add an item to this list.")

;;; Internal state for the current `desire' invocation
(defvar desire-precondition nil
  "*Precondition for package loading.")
(defvar desire--recipe-type "package"
  "Active package-manager type: \"package\", \"quelpa\" or \"straight\".")
(defvar desire--straight-recipe nil
  "Active straight.el recipe (PACKAGE . PLIST) for the current call.")
(defvar desire--quelpa-recipe nil
  "Active quelpa recipe (PACKAGE . PLIST) for the current call.")
(defvar desire--vc-recipe nil
  "Active package-vc recipe (PACKAGE . PLIST) for the current call.")
(defvar desire--vc-plist nil
  "Normalised recipe plist for the current `desire' call.")
(defvar desire--fname nil
  "Internal: trigger file name used by `desire'.")
(defvar desire--precond nil
  "Internal: precondition library used by `desire'.")

;;; Helpers
(defvar desire-debug nil
  "When non-nil, `desire' logs verbose diagnostic messages.
Set this via `setq' or `customize' when troubleshooting package
configuration.")

(defvar desire--load-path-warned nil
  "Set to t once the empty-`desire-load-path' warning has been shown.")

(defmacro desire--debug (fmt &rest args)
  "Log FMT/ARGS via `message' when `desire-debug' is non-nil."
  (declare (indent 1) (debug t))
  `(when desire-debug
     (message ,fmt ,@args)))

(defun desire--check-load-path ()
  "Warn (once) if `desire-load-path' is empty."
  (when (and (null desire-load-path)
             (not desire--load-path-warned))
    (setq desire--load-path-warned t)
    (display-warning 'desire
                     "`desire-load-path' is nil; no configuration files will be loaded."
                     :warning)))

;;; package-vc backend
(defvar desire-vc-keywords
  '(:fetcher :repo :rev :backend)
  "All arguments that `package-vc-install' supports.")

(defconst desire-vc-fetchers
  '(:github "https://github.com/"
	    :gitlab "https://gitlab.com/"
	    :codeberg "https://codeberg.org/"
	    :sourcehut "https://git.sr.ht/~")
  "Places from where to fetch packages.")

(cl-defun desire-vc--install (package &key fetcher repo url name rev backend
                                      &allow-other-keys)
  "Install PACKAGE from a package-vc recipe.
URL, if given, is used verbatim as the source URL; otherwise FETCHER
and REPO are concatenated.  REV selects the revision, BACKEND the VC
backend, NAME overrides PACKAGE as the package name."
  (package-vc-install (or url (concat fetcher repo))
                      rev backend (or name package)))

(defun desire-vc--check-fetcher (val)
  "Check whether VAL is a correct `:fetcher' argument.
More specifically, check if it's (i) URL or (ii) either a string
or a symbol representing one possible destination in
`vc-use-package-keywords'."
  (cond
   ((string-prefix-p "https://" val) val)
   ((plist-get desire-vc-fetchers (intern (concat ":" val))))
   (t (error ":fetcher is not a url or one of %s."
             (mapcar #'car desire-vc-fetchers)))))

(defun desire-vc--normalise-args (args)
  "Normalise the plist for vc-recipe."
  (cl-flet* ((mk-string (s)
	       (if (stringp s) s (symbol-name s)))
	     (normalise (arg val)
	       (pcase arg
		 (:fetcher (desire-vc--check-fetcher (mk-string val)))
		 (:rev (if (eq val :last-release) val (mk-string val)))
		 (:repo (mk-string val))
		 (_ val))))
    (cl-loop for (k v) on args by #'cddr
	     nconc (list k (normalise k v)))))

;; (defun desire-vc--recipe-format (args)
;;   "Convert to package-vc format"
;;   desire-vc--normalise-args args
;;   )

;;; }}}



;; (defun desired (package &optional fname precond)
(cl-defun desired (package
		   &key precondition-lisp-library precondition-system-executable)
  "Add PACKAGE (a symbol) as something which is `desirable'.
  The optional argument FNAME is a string containing
  the name of the file that, when loaded, will
  trigger dynamic loading of extra configuration files. If FNAME is
  omitted then the string corresponding to PACKAGE is used instead.
  PRECOND is name of file as precondition for package loadind."

  (or (symbolp package)
      (error "Wrong type argument to `desired': symbolp, %s"
	     (prin1-to-string package))
      )

  ;; Message: found executable precondition
  (if precondition-system-executable
      (if (executable-find precondition-system-executable)
	  (message "Executable file found %s to load package %s"
		   (prin1-to-string precondition-system-executable)
		   (prin1-to-string package))
	(message "Cannot find executable %s to load package %s"
		 (prin1-to-string precondition-system-executable)
		 (prin1-to-string package)))
    nil)

  ;; Check precondition
  (if (and
       (not (desiredp package))
       (if precondition-lisp-library
	   (if (stringp precondition-lisp-library)
	       (locate-library precondition-lisp-library)
	     nil)
	 t)
       (if precondition-system-executable
	   (if (stringp precondition-system-executable)
	       (executable-find precondition-system-executable)
	     nil)
	 t))
      (add-to-list 'desirable (symbol-name package))
    nil
    )
  )

(defun desiredp (package)
  "Return t if PACKAGE is `desirable'.  PACKAGE can be a symbol or a string."
  (let ((p (or
	    (and (symbolp package) (symbol-name package))
	    (and (stringp package) package)
	    (error
	     "Wrong type argument to `desiredp': symbolp or stringp, %s"
	     (prin1-to-string package)))))
    (and (member p desirable) t)))

(defun desire-conf (package &optional fname precond autoinstall)
  "Arrange loading and configuration of a desired emacs PACKAGE.
PACKAGE is a symbol representing the name of a package.  The aim is to
set up some autoloads and other initial configuration, and possibly
organise for more configuration files to be dynamically loaded when
the package itself is finally loaded.  The optional argument FNAME is
a string containing the name of the file that, when loaded, will
trigger dynamic loading of extra configuration files. If FNAME is
omitted then the string corresponding to PACKAGE is used instead.
PRECOND is name of file as precondition for package loadind.

AUTOINSTALL: if equal 't then package autoinstalled.

Each directory in `desire-load-path' is searched in order to see if
configuration data for PACKAGE exists.  The configuration data takes
one of 2 forms:

1. A file named PACKAGE followed by the value of the variable
   `desire-extension'.  If such a file exists, then that file is taken
   to be the sole configuration file for the package.  This file is
   loaded immediately.  The file might contain autoloads or might load
   the package itself.

2. A directory named PACKAGE. If the directory contains a file
   corresponding to `desire-loaddefs' then that file is loaded
   immediately.  Other files in the directory are processed using the
   function `desire-process-directory' after the package is actually
   loaded.

Note that a single file, as described in (1), takes precedence over a
directory, as described in (2).  For a directory, if the package has
already been loaded then the configuration data *is* loaded but the
package is *not*.  This depends on the `eval-after-load' function
behaving similarly.

If a package is successfully configured then it is `desired' and,
therefore, marked as `desirable'.  Desirability can be checked using
the function `desiredp'.  If PACKAGE has been previously `desired'
then nothing happens and nil is returned."

  (or (symbolp package)
      (error "Wrong type argument to `desire': symbolp, %s"
	     (prin1-to-string package)
	     )
      )

  ;; check autoinstall key
  (if autoinstall
      ;; install package
      (desire-install-package package)
    nil
    )

  ;; Check precondition
  (if (and
       (not (desiredp package))
       (if precond
	   (if (stringp precond)
	       (locate-library precond)
	     nil
	     )
	 t
	 )
       )
      (let*
	  (
	   (dirs desire-load-path)
	   (pname (symbol-name package))
	   (lname (if fname fname pname))
	   )

	(while dirs

	  (let ((prefix (expand-file-name pname (car dirs))))

	    (cond
	     ;;{{{ Check for configuration file

	     (
	      (desire-readable-regular-file-p
	       (concat prefix desire-extension)
	       )

	      ;; Load configuration data.
	      (desire-load-file (car dirs) pname)

	      ;; Finished!
	      (desired package)
					;(setq dirs nil)
	      (setq dirs (cdr dirs))
	      )

	     ;;}}}
	     ;;{{{ Check for configuration directory

	     (
	      (and (file-directory-p prefix)
		   (file-readable-p prefix)
		   )

	      ;; If file specified by desire-loaddefs exists then load it.
	      (if (and desire-loaddefs
		       (desire-readable-regular-file-p
			(expand-file-name
			 (concat desire-loaddefs desire-extension)
			 prefix)))

		  (desire-load-file prefix desire-loaddefs)
		)

	      ;; Setup processing of directory.
	      (eval-after-load
		  lname
		`(desire-process-directory ,prefix))

	      ;; Finished!
	      (desired package)
					;(setq dirs nil)
	      (setq dirs (cdr dirs))
	      )

	     ;;}}}
	     ;;{{{ Otherwise

	     (t
	      (setq dirs (cdr dirs))
	      )

	     ;;}}}
	     ) ; end cond
	    ) ; end let
	  ) ; end while
	) ; end let*
    ) ; end if
  ) ; end defun desire

(cl-defun desire (package
		  &key initname precondition-lisp-library precondition-system-executable ensurename (ensure desire-package-autoinstall) pkgman recipe)
  "Arrange loading and configuration of a desired emacs PACKAGE.
PACKAGE is a symbol representing the name of a package.  The aim is to
set up some autoloads and other initial configuration, and possibly
organise for more configuration files to be dynamically loaded when
the package itself is finally loaded.

Accepts the following optional arguments:

Accepts the following properties:

 :initname FNAME
    FNAME is a string containing the name of the file that, when loaded, will
    trigger dynamic loading of extra configuration files. If FNAME is
    omitted then the string corresponding to PACKAGE is used instead.
 :precondition PRECOND
    PRECOND is name of lisp file as precondition for package loadind.
 :executable EXECUTABLE
    EXECUTABLE is name of executable file as precondition for package loadind.
 :ensurename PACKAGE-NAME Name of package on repository (like melpa).
 :autoinstall BOOL
    Specifies package autoinstallation.
    `t' - package autoinstalled.
    `nil' - package not autoinstalled.
    Default from `desire-package-autoinstall' variable.
 :pkgman PACKAGE-MANAGER
   Specifies a package-manager.
   default: package
   supported: package, quelpa, straight
 :recipe RECIPE
   Specifies a straight.el recipe to allow you to acquire packages from external
   sources. See https://github.com/raxod502/straight.el#the-recipe-format for
   details on this recipe.

Each directory in `desire-load-path' is searched in order to see if
configuration data for PACKAGE exists.  The configuration data takes
one of 2 forms:

1. A file named PACKAGE followed by the value of the variable
   `desire-extension'.  If such a file exists, then that file is taken
   to be the sole configuration file for the package.  This file is
   loaded immediately.  The file might contain autoloads or might load
   the package itself.

2. A directory named PACKAGE. If the directory contains a file
   corresponding to `desire-loaddefs' then that file is loaded
   immediately.  Other files in the directory are processed using the
   function `desire-process-directory' after the package is actually
   loaded.

Note that a single file, as described in (1), takes precedence over a
directory, as described in (2).  For a directory, if the package has
already been loaded then the configuration data *is* loaded but the
package is *not*.  This depends on the `eval-after-load' function
behaving similarly.

If a package is successfully configured then it is `desired' and,
therefore, marked as `desirable'.  Desirability can be checked using
the function `desiredp'.  If PACKAGE has been previously `desired'
then nothing happens and nil is returned."

  (or (symbolp package)
      (error "Wrong type argument to `desire': symbolp, %s"
	     (prin1-to-string package)))

  ;; Set initial file/directory name
  (setq desire--fname initname)

  (message "initname: %s" initname)
  (message "fname: %s" desire--fname)

  ;; Set lisp library precondition
  (setq desire--precond
	(if precondition-lisp-library
            precondition-lisp-library
          (prin1-to-string package)))

  (message "precond: %s" desire--precond)

;;; Set package manager
  (setq desire--recipe-type
	(cond ((null pkgman) "package")
              ((symbolp pkgman) (symbol-name pkgman))
              ((stringp pkgman) pkgman)
              (t (error "Unsupported :pkgman %S" pkgman))))

  (message "recipe-type: %s" desire--recipe-type)

  ;; Use recipe
  (when recipe
    (cond
     ;; straight-recipe
     ((equal desire--recipe-type "straight")
      (setq desire--straight-recipe (cons package recipe)))
     ;; quelpa-recipe
     ((equal desire--recipe-type "quelpa")
      (setq desire--quelpa-recipe (cons package recipe)))
     ;; vc-recipe
     ((equal desire--recipe-type "package")
      (setq desire--vc-plist (desire-vc--normalise-args recipe))
      (setq desire--vc-recipe (cons package desire--vc-plist)))))

  (when recipe
    (cond
     ((equal desire--recipe-type "straight")
      (message "straight-recipe: package = %s ; recipe = %s ; straight-recipe = %s"
               package recipe desire--straight-recipe))
     ((equal desire--recipe-type "quelpa")
      (message "quelpa-recipe: package = %s ; recipe = %s ; quelpa-recipe = %s"
               package recipe desire--quelpa-recipe))
     ((equal desire--recipe-type "package")
      (message "vc-recipe: package = %s ; recipe = %s ; vc-recipe = %s"
               package recipe desire--vc-recipe))))

  ;; Check ensure key
  (when ensure
    ;; check if the package is already installed
    (if (or (package-installed-p package)
            (locate-library (symbol-name package)))
	t
      ;; install package
      (if ensurename
          (desire-install-package ensurename recipe)
	(desire-install-package package recipe))))

  (message "ensure: %s : %s; ensurename = %s " package ensure ensurename)

;;; Message: found executable precondition
  (if precondition-system-executable
      (if (executable-find precondition-system-executable)
	  (message "Executable file found %s to load package %s"
		   (prin1-to-string precondition-system-executable)
		   (prin1-to-string package))
	(message "Cannot find executable %s to load package %s"
		 (prin1-to-string precondition-system-executable)
		 (prin1-to-string package)))
    nil)

;;; Check precondition
  (if (and
       (not (desiredp package))
       (if desire--precond
           (and (stringp desire--precond)
		(locate-library desire--precond))
	 t)
       (if precondition-system-executable
	   (if (stringp precondition-system-executable)
	       (executable-find precondition-system-executable)
	     nil)
	 t))
      (let*
	  ((dirs desire-load-path)
	   (pname (symbol-name package))
	   (lname (if desire--fname desire--fname pname)))

	(message "precondition: %s" precondition-lisp-library)
	(message "precond: %s" desire--precond)

	;; ;; Check ensure key
	;; (if ensure
	;;	  ;; check if the package is already installed
	;;	  (if (or
	;;	       (if (stringp package)
	;;		   (locate-library package)
	;;		 nil)
	;;	       (if (stringp precond)
	;;		   (locate-library precond)
	;;		 nil)
	;;	       )
	;;	      t
	;;	    ;; install package
	;;	    (if ensurename
	;;		(desire-install-package ensurename)
	;;	      (desire-install-package package))
	;;	    ))

	;; (message "ensure: %s : %s; ensurename = %s " package ensure ensurename)

	;;Test
	;; unconditional desirable
	;; Возможно, нужно внести дополнительный ключ
	(desired package)

	(while dirs

	  (let ((prefix (expand-file-name pname (car dirs))))

	    (cond
	   ;;; Check for configuration file
	     ((desire-readable-regular-file-p
	       (concat prefix desire-extension))

	    ;;; Load configuration data.
	      (desire-load-file (car dirs) pname)

	      ;; Finished!
	      (desired package)
	      ;;(setq dirs nil)
	      (setq dirs (cdr dirs)))

	   ;;; Check for configuration directory
	     ((and (file-directory-p prefix)
		   (file-readable-p prefix))

	    ;;; If file specified by desire-loaddefs exists then load it.
	      (if (and desire-loaddefs
		       (desire-readable-regular-file-p
			(expand-file-name
			 (concat desire-loaddefs desire-extension)
			 prefix)))

		  (desire-load-file prefix desire-loaddefs))

	      ;; Setup processing of directory
	      (eval-after-load
		  lname
		`(desire-process-directory ,prefix))

	      ;; Finished!
	      (desired package)
	      ;;(setq dirs nil)
	      (setq dirs (cdr dirs)))

	     ;; Otherwise

	     (t
	      (setq dirs (cdr dirs)))
	     ) ;; end cond
	    ) ;; end let
	  ) ;; end while
	) ;; end let*
    ) ;; end if
  ) ;; end defun desire

(defun desire-load-file (dir file)
  "Load FILE from DIR after appending `desire-extension'."

  (load-file
   (expand-file-name (concat file desire-extension) dir))
  )

(defun desire-load-dir (prefix dir)
  "Load files from PREFIX/DIR after appending `desire-dir-extension'."

  (let
      ((dir-list))
    (setq dir-list
	  (directory-files
	   (expand-file-name (concat dir desire-dir-extension) prefix)
	   t
	   (concat ".*" (regexp-quote desire-extension) "$")))
    (if (not (null dir-list))
	(mapcar 'load-file
		dir-list))
    ) ;; end let
  )

(defun desire-process-directory (dir)

  "Load files in DIR if they correspond to desirable packages.

If a configuration file for a previously desired package is present in
DIR, then that file is loaded.  For example, if a package called
\"goodthing\" has previously been desired, then if a file exists in
DIR called \"goodthing\" (plus `desire-extension') it will be loaded.
If \"goodthing\" has not previously been desired, then this file will
be ignored.

A file for the pseudo-package specified by `desire-desire' is always
loaded first.  A file for the pseudo-package specified by
`desire-execute' is always loaded last."

  (let
      ((fs (desire-directory-file-prefixes dir))
       (ds (desire-directory-dir-prefixes dir))
       exec)

    ;; Check for the desire-desire file.
    (if (member desire-desire fs)

	(progn
	  (desire-load-file dir desire-desire)
	  (setq fs (delete desire-desire fs))))

    ;; Check for the desire-desire dir.
    (if (member desire-desire ds)

	(progn
	  (desire-load-dir dir desire-desire)
	  (setq ds (delete desire-desire ds))))

    ;; Check for the desire-execute file.
    (if (member desire-execute fs)
	(progn
	  (setq exec t)
	  (setq fs (delete desire-execute fs))))

    ;; Load desirable files.
    (while fs
      (let ((hd (car fs)))

	;; Check to see if the prefix represents a feature.
	(if (desiredp hd)
	    (desire-load-file dir hd)))
      (setq fs (cdr fs)))

    ;; Load desirable dirs.
    (while ds
      (let ((hd (car ds)))

	;; Check to see if the prefix represents a feature.
	(if (desiredp hd)
	    (desire-load-dir dir hd)))

      (setq ds (cdr ds)))

    ;; Load desire-execute if required.
    (if exec
	(desire-load-file dir desire-execute))
    )
  )

(defun desire-directory-file-prefixes (dir)

  "Return the prefixes of configuration files in DIR.
Configuration files are those that readable, regular files which have
the extension specified by variable `desire-extension'.  The prefix
is the file name with the directory and extension removed."

  (let
      (
       (ext-regexp (concat (regexp-quote desire-extension) "$"))
       (out)
       )

    (apply 'append
	   (mapcar
	    (function
	     (lambda (f)
	       (let ((full (expand-file-name f dir)))
		 (if (and
		      (desire-readable-regular-file-p full)
		      (string-match ext-regexp f))
		     (list (substring f 0 (string-match ext-regexp f)))))))
	    (directory-files dir))
	   )
    )
  )

(defun desire-directory-dir-prefixes (dir)

  ;;{{{ Description

  "Return the prefixes of configuration directories in DIR.
Configuration directories are those that readable directories which have
the extension specified by variable `desire-dir-extension'.  The prefix
is the directory name with the prefix directory and extension removed."

  ;;}}}

  (let
      (
       (ext-regexp (concat (regexp-quote desire-dir-extension) "$"))
       (out)
       )

    (apply 'append
	   (mapcar
	    (function
	     (lambda (f)
	       (let ((full (expand-file-name f dir)))
		 (if (and
		      (desire-readable-dir-p full)
		      (string-match ext-regexp f)
		      )
		     (list (substring f 0 (string-match ext-regexp f)))
		   )
		 )
	       )
	     ) ;; end function
	    (directory-files dir)
	    )
	   )
    ) ;; end let
  )

(defun desire-readable-regular-file-p (f)

  "Determine if F is a readable, regular file."

  (and
   (if (fboundp 'file-regular-p)
       (file-regular-p f)
     t)
   (file-readable-p  f)
   )
  )

(defun desire-readable-dir-p (dir)
  "Determine if DIR is a readable directory."
  (and
   (file-directory-p dir)
   (file-readable-p dir)))

(defun desire-require (feature &optional fname)
  "Work like function require.
  If feature FEATURE is not loaded, load it from FNAME.
  If FEATURE is not a member of the list `features', then the feature
  is not loaded; so load the file FNAME."
  (if (not fname)
      (setq fname (prin1-to-string feature)))
  (if (require feature fname 1)
      t
    (message "Package not found : %s" fname)))

;; (defun desire-install-package (package recipe)
;;   "Install PACKAGE from repository"
;;   (unless (package-installed-p package)
;;     (if recipe
;;	(straight-use-package quelpa-recipe)
;;       (progn
;;	(package-refresh-contents)
;;	(package-install package))
;;       )))

(defun desire-install-package (package &optional recipe)
  "Install PACKAGE from repository.
RECIPE, when non-nil, selects a non-default install path according to
`desire--recipe-type'.  `package-vc' recipes are delegated to
`desire-vc--install'."
  (unless (package-installed-p package)
    (if recipe
        (pcase desire--recipe-type
          ("quelpa"   (quelpa desire--quelpa-recipe))
          ("straight" (straight-use-package desire--straight-recipe))
          ("package"  (apply #'desire-vc--install
                             package
                             (cdr desire--vc-recipe))))
      (package-refresh-contents)
      (package-install package))))

(provide 'desire)

;;;
