;;; -*- mode: emacs-lisp; lexical-binding: t; coding: utf-8-unix; -*-
;;; desire.el --- versatile configuration for emacs lisp packages

;; Authors:         Martin Schwenke <martin@meltin.net>
;;                  Graham Williams <Graham.Williams@cmis.csiro.au>
;;		    Dmitry S. Kulyabov <yamadharma@gmail.com>
;; Maintainer:      Dmitry S. Kulyabov <yamadharma@gmail.com>
;; Created:         20-Jun-1995

;; Keywords: setup configuration
;; Package-Requires: ((emacs "29.1") (cl-lib "0.5"))

;; Copyright (C) 1995-2001 Martin Schwenke and Graham Williams
;; Copyright (C) 2002-2026 Dmitry S. Kulyabov

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
;; Backend modules (`desire-straight', `desire-quelpa') are loaded
;; lazily by `desire--ensure-backend' -- see below.  This keeps
;; `(require 'desire)' free of network I/O and optional dependencies.

(defvar desire-load-path nil
  "*List of directories to be searched by `desire' for configuration data.")

(defconst desire-extension ".ecf"
  "The extension given to configuration files used by `desire'.")

(defconst desire-dir-extension ".ecd"
  "The extension given to configuration directories used by `desire'.")

(defconst desire-loaddefs "loaddefs"
  "Name of a configuration file that is loaded before a package itself
is loaded.  Useful for autoloading functions.")

(defconst desire-desire "desire"
  "Name of a configuration file that is loaded after a package itself
is loaded, but before the configuration files for any other desirable
package.  Useful for performing some initial setup.")

(defconst desire-execute "execute"
  "Name of a configuration file that is loaded after a package itself
is loaded, and also after the configuration files for any other
desirable package.  Useful for performing some final setup.")

(defconst desire-postinstall "postinstall"
  "*A string which is the name of a configuration file that is loaded
after a package itself is installed. This is useful for performing
some final setup.")

(defvar desire-precondition nil
  "*Precondition for package loading.")

(defvar desire-package-autoinstall t
  "Enable automatic installation of a package if it is not present on the system.")

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

(cl-defun desired (package
		   &key precondition-lisp-library precondition-system-executable)
  "Mark PACKAGE (a symbol) as desirable.
PACKAGE is added to the list `desirable' if:
  - PRECONDITION-LISP-LIBRARY, when non-nil, is a string naming a
    library that must be locatable via `locate-library', and
  - PRECONDITION-SYSTEM-EXECUTABLE, when non-nil, is a string naming
    an executable that must be findable via `executable-find'.
If PACKAGE is already `desirable', this is a no-op.  Always returns
nil; use `desiredp' to query the state."

  (declare (indent defun))

  (or (symbolp package)
      (error "Wrong type argument to `desired': symbolp, %s"
	     (prin1-to-string package)))

  ;; Message: found executable precondition
  (when precondition-system-executable
    (if (executable-find precondition-system-executable)
        (message "Executable file found %s to load package %s"
                 precondition-system-executable
                 package)
      (message "Cannot find executable %s to load package %s"
               precondition-system-executable
               package)))

  ;; Check precondition
  (if (and
       (not (desiredp package))
       (if precondition-lisp-library
	   (if (stringp precondition-lisp-library)
	       (locate-library precondition-lisp-library)
	     nil)
	 t)
       (if precondition-system-executable
           (and (stringp precondition-system-executable)
		(executable-find precondition-system-executable))
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
  "Deprecated positional wrapper around `desire'.
Kept for backward compatibility with older configurations; new code
should call `desire' with keyword arguments instead.

Original behaviour:
  PACKAGE     – symbol; the package to configure.
  FNAME       – init file name (defaults to PACKAGE's name).
  PRECOND     – lisp library precondition (no default check).
  AUTOINSTALL – when non-nil, install PACKAGE if not installed.

Equivalent modern call:
  (desire PACKAGE
          :initname FNAME
          :precondition-lisp-library PRECOND
          :no-precondition-check (null PRECOND)
          :ensure AUTOINSTALL)"
  (declare (advertised-calling-convention
            (package &rest args)
            "desire.el 2025"))
  (desire package
          :initname fname
          :precondition-lisp-library precond
          :no-precondition-check (null precond)
          :ensure autoinstall))

(cl-defun desire (package
		  &key initname precondition-lisp-library precondition-system-executable
		  ensurename (ensure desire-package-autoinstall)
		  pkgman recipe no-precondition-check)
  "Arrange loading and configuration of a desired emacs PACKAGE.
PACKAGE is a symbol representing the name of a package.  The aim is to
set up some autoloads and other initial configuration, and possibly
organise for more configuration files to be dynamically loaded when
the package itself is finally loaded.

Accepts the following keyword arguments:

 :initname FNAME
    FNAME is a string containing the name of the file that, when loaded,
    will trigger dynamic loading of extra configuration files.  If FNAME
    is omitted then the string corresponding to PACKAGE is used instead.

 :precondition-lisp-library LIB
    LIB is the name of a lisp library (a string) that must be locatable
    via `locate-library' before configuration files for PACKAGE are
    loaded.  Defaults to PACKAGE's name, so configuration is only loaded
    when the package itself can already be found; this prevents a `.ecf'
    file that `require's the package from failing when the package is
    not installed.

 :precondition-system-executable EXEC
    EXEC is the name of an executable (a string) that must be present
    (found via `executable-find') before configuration files for PACKAGE
    are loaded.  A warning is issued if EXEC cannot be found.

 :ensurename NAME
    NAME (a symbol or string) is the name under which the package is
    known to the package manager (e.g. on MELPA).  Used only when
    `:ensure' is non-nil.

 :ensure BOOL
    When non-nil, ensure PACKAGE is installed before loading its
    configuration.  Defaults to the value of
    `desire-package-autoinstall'.  Pass `:ensure nil' to suppress
    auto-installation for a single call.

 :pkgman MANAGER
    Package manager to use when installing: `package' (default),
    `quelpa' or `straight'.

 :recipe RECIPE
    Recipe understood by the selected `:pkgman'.  For `:pkgman package'
    the recipe is a plist with keys recognised by
    `desire-vc--normalise-args' (`:fetcher', `:repo', `:rev',
    `:backend').  For straight/quelpa see the respective documentation.

 :no-precondition-check BOOL
    Internal.  When non-nil, skip the `:precondition-lisp-library'
    check entirely.  Used by the deprecated `desire-conf' wrapper to
    preserve its historical behaviour.  Do not use in new code.

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

  (declare (indent defun))

  (or (symbolp package)
      (error "Wrong type argument to `desire': symbolp, %s"
	     (prin1-to-string package)))

  (desire--check-load-path)

  ;; Set initial file/directory name
  (setq desire--fname initname)

  ;; Set lisp library precondition.
  ;;
  ;; Priority order:
  ;;   1. :no-precondition-check t      -> no check at all
  ;;   2. :precondition-lisp-library X  -> must be locatable
  ;;   3. default                       -> require PACKAGE itself
  ;;
  ;; Case 3 is what most users want: a `.ecf' for PACKAGE should only
  ;; run when PACKAGE is installed.  Case 1 is used by the deprecated
  ;; `desire-conf' wrapper to preserve its historical "no check" default.
  (setq desire--precond
	(cond
	 (no-precondition-check nil)
	 (precondition-lisp-library precondition-lisp-library)
	 (t (symbol-name package))))

  (desire--debug "precond: %s" desire--precond)

;;; Set package manager
  (setq desire--recipe-type
	(cond ((null pkgman) "package")
	      ((symbolp pkgman) (symbol-name pkgman))
	      ((stringp pkgman) pkgman)
	      (t (error "Unsupported :pkgman %S" pkgman))))

  (desire--debug "recipe-type: %s" desire--recipe-type)

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
      (desire--debug "straight-recipe: package = %s ; recipe = %s ; straight-recipe = %s"
                     package recipe desire--straight-recipe))
     ((equal desire--recipe-type "quelpa")
      (desire--debug "quelpa-recipe: package = %s ; recipe = %s ; quelpa-recipe = %s"
                     package recipe desire--quelpa-recipe))
     ((equal desire--recipe-type "package")
      (desire--debug "vc-recipe: package = %s ; recipe = %s ; vc-recipe = %s"
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

  (desire--debug "ensure: %s : %s; ensurename = %s" package ensure ensurename)

;;; Executable precondition diagnostics
  (when precondition-system-executable
    (if (executable-find precondition-system-executable)
	(desire--debug "Executable file found %s to load package %s"
                       precondition-system-executable
                       package)
      (display-warning
       'desire
       (format "Cannot find executable %s required for package %s"
               precondition-system-executable
               package)
       :warning)))

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

	(desire--debug "precondition: %s" precondition-lisp-library)
	(desire--debug "precond: %s" desire--precond)

	;; NOTE: PACKAGE is deliberately *not* marked `desired' here.
	;; It is marked inside each successful branch of the `cond' below,
	;; so that a package whose configuration could not be found stays
	;; un-desired and can be retried later.  This matches `desire-conf'.
	;; (desired package)

	(while dirs
	  (let ((prefix (expand-file-name pname (car dirs))))

	    (cond
	   ;;; Check for configuration file
	     ((desire-readable-regular-file-p
	       (concat prefix desire-extension))

	    ;;; Load configuration data
	      (desire-load-file (car dirs) pname)

	      ;; Finished!
	      (desired package)
	      (setq dirs nil))

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

	      ;; Setup processing of directory.
	      (eval-after-load
		  lname
		`(desire-process-directory ,prefix))

	      ;; Finished!
	      (desired package)
	      (setq dirs nil))

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
  (load-file (expand-file-name (concat file desire-extension) dir)))

(defun desire-load-dir (prefix dir)
  "Load files from PREFIX/DIR after appending `desire-dir-extension'."
  (let ((full-dir (expand-file-name (concat dir desire-dir-extension)
                                    prefix))
        (regexp (concat (regexp-quote desire-extension) "$")))
    (dolist (file (directory-files full-dir t regexp))
      (load-file file))))

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
       exec-file
       exec-dir)

    ;; Check for the desire-desire file.
    (when (member desire-desire fs)
      (desire-load-file dir desire-desire)
      (setq fs (delete desire-desire fs)))

    ;; Check for the desire-desire dir.
    (when (member desire-desire ds)
      (desire-load-dir dir desire-desire)
      (setq ds (delete desire-desire ds)))

    ;; Check for the desire-execute file and dir (both supported).
    (when (member desire-execute fs)
      (setq exec-file t)
      (setq fs (delete desire-execute fs)))
    (when (member desire-execute ds)
      (setq exec-dir t)
      (setq ds (delete desire-execute ds)))

    ;; Load desirable files.
    (dolist (f fs)
      (when (desiredp f)
	(desire-load-file dir f)))

    ;; Load desirable dirs.
    (dolist (d ds)
      (when (desiredp d)
	(desire-load-dir dir d)))

    ;; Load desire-execute last (file then dir).
    (when exec-file
      (desire-load-file dir desire-execute))
    (when exec-dir
      (desire-load-dir dir desire-execute))))

(defun desire-directory-file-prefixes (dir)
  "Return the prefixes of configuration files in DIR.
Configuration files are those that are readable, regular files with
the extension specified by `desire-extension'.  The prefix is the file
name with the directory and extension removed."
  (let ((ext-regexp (concat (regexp-quote desire-extension) "$")))
    (cl-loop for f in (directory-files dir)
             for full = (expand-file-name f dir)
             when (and (desire-readable-regular-file-p full)
                       (string-match ext-regexp f))
             collect (substring f 0 (string-match ext-regexp f)))))

(defun desire-directory-dir-prefixes (dir)
  "Return the prefixes of configuration directories in DIR.
Configuration directories are those that are readable directories with
the extension specified by `desire-dir-extension'.  The prefix is the
directory name with the parent directory and extension removed."
  (let ((ext-regexp (concat (regexp-quote desire-dir-extension) "$")))
    (cl-loop for f in (directory-files dir)
             for full = (expand-file-name f dir)
             when (and (desire-readable-dir-p full)
                       (string-match ext-regexp f))
             collect (substring f 0 (string-match ext-regexp f)))))

(defun desire-readable-regular-file-p (f)
  "Determine if F is a readable, regular file."
  (and (file-regular-p f)
       (file-readable-p f)))

(defun desire-readable-dir-p (dir)
  "Determine if DIR is a readable directory."
  (and (file-directory-p dir)
       (file-readable-p dir)))

(defun desire-require (feature &optional fname)
  "Work like function require.
If FEATURE is not loaded, load it from FNAME.  If FEATURE is not a
member of `features', load the file FNAME instead."
  (unless fname
    (setq fname (symbol-name feature)))
  (or (require feature fname 1)
      (message "Package not found: %s" fname)))

(defun desire--ensure-backend (recipe-type)
  "Ensure the backend for RECIPE-TYPE is available.
RECIPE-TYPE is a string, one of \"package\", \"quelpa\", \"straight\".
Backend modules are loaded lazily on first use."
  (pcase recipe-type
    ("straight" (require 'desire-straight)
     (desire-straight-ensure))
    ("quelpa"   (require 'desire-quelpa)
     (desire-quelpa-ensure))
    ("package"  nil)
    (_ (error "Unknown :pkgman %S" recipe-type))))

(defun desire-install-package (package &optional recipe)
  "Install PACKAGE from repository.
RECIPE, when non-nil, selects a non-default install path according to
`desire--recipe-type'.  Package managers (`straight', `quelpa') are
bootstrapped, not installed via `package.el'."
  (unless (package-installed-p package)
    (cond
     ;; Backends themselves: bootstrap, don't try to install from ELPA.
     ((eq package 'straight)
      (require 'desire-straight)
      (desire-straight-ensure))
     ((eq package 'quelpa)
      (require 'desire-quelpa)
      (desire-quelpa-ensure))
     ;; Recipe-driven install.
     (recipe
      (desire--ensure-backend desire--recipe-type)
      (pcase desire--recipe-type
        ("quelpa"   (desire-quelpa-use-package desire--quelpa-recipe))
        ("straight" (desire-straight-use-package desire--straight-recipe))
        ("package"  (apply #'desire-vc--install
                           package
                           (cdr desire--vc-recipe)))))
     ;; Plain package.el fallback.
     (t
      (package-refresh-contents)
      (package-install package)))))

(provide 'desire)

;;;
