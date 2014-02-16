;;; quelpa.el --- Emacs Lisp packages built directly from source

;; Copyright 2014, Steckerhalter
;; Copyright 2014, Vasilij Schneidermann <v.schneidermann@gmail.com>

;; Author: steckerhalter
;; URL: https://github.com/quelpa/quelpa
;; Version: 0.0.1
;; Package-Requires: ((package-build "0"))
;; Keywords: package management build source elpa

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Your personal local Emacs Lisp Package Archive (ELPA) with packages
;; built on-the-fly directly from source.

;; See the README.org for more info:
;; https://github.com/steckerhalter/quelpa/README.org

;;; Requirements:

;; Emacs 24.

;;; Code:

(require 'package-build)
(require 'cl-lib)

;; --- customs / variables ---------------------------------------------------

(defgroup quelpa nil
  "Build and install packages from source code"
  :group 'package)

(defcustom quelpa-dir (expand-file-name (concat user-emacs-directory "quelpa"))
  "Where quelpa builds and stores packages."
  :group 'quelpa
  :type 'string)

(defcustom quelpa-build-dir (concat quelpa-dir "/build")
  "Where quelpa builds packages."
  :group 'quelpa
  :type 'string)

(defcustom quelpa-packages-dir (concat quelpa-dir "/packages")
  "Where quelpa buts built packages."
  :group 'quelpa
  :type 'string)

(defcustom quelpa-melpa-dir (concat quelpa-dir "/melpa")
  "Where melpa is checked out (to get the recipes)."
  :group 'quelpa
  :type 'string)

(defcustom quelpa-before-hook '(quelpa-init)
  "List of functions to be called before quelpa."
  :group 'quelpa
  :type 'hook)

(defcustom quelpa-after-hook '(quelpa-shutdown)
  "List of functions to be called after quelpa."
  :group 'quelpa
  :type 'hook)

(defvar quelpa-initialized-p nil
  "Non-nil when quelpa has been initialized.")

(defvar quelpa-force-update-p nil
  "When non-nil, `quelpa' will force an update.")

;; --- compatibility for legacy `package.el' in Emacs 24.3  -------------------

(defun quelpa-setup-package-structs ()
  "Setup the struct `package-desc' when not available.
`package-desc-from-legacy' is provided to convert the legacy
vector desc into a valid PACKAGE-DESC."
  (unless (functionp 'package-desc-p)
    (cl-defstruct
        (package-desc
         (:constructor
          ;; convert legacy package desc into PACKAGE-DESC
          package-desc-from-legacy
          (pkg-info kind
                    &aux
                    (name (intern (aref pkg-info 0)))
                    (version (version-to-list (aref pkg-info 3)))
                    (summary (if (string= (aref pkg-info 2) "")
                                 "No description available."
                               (aref pkg-info 2)))
                    (reqs  (aref pkg-info 1))
                    (kind kind))))
      name
      version
      (summary "No description available.")
      reqs
      kind
      archive
      dir
      extras
      signed)))

;; --- package building ------------------------------------------------------

(defun quelpa-package-type (file)
  "Determine the package type of FILE.
Return `tar' for tarball packages, `single' for single file
packages, or nil, if FILE is not a package."
  (let ((ext (file-name-extension file)))
    (cond
     ((string= ext "tar") 'tar)
     ((string= ext "el") 'single)
     (:else nil))))

(defun quelpa-get-package-desc (file)
  "Extract and return the PACKAGE-DESC struct from FILE.
On error return nil."
  (let* ((kind (quelpa-package-type file))
         (desc (with-demoted-errors "Error getting PACKAGE-DESC: %s"
                 (with-temp-buffer
                   (insert-file-contents-literally file)
                   (pcase kind
                     (`single (package-buffer-info))
                     (`tar (tar-mode)
                           (if (help-function-arglist 'package-tar-file-info)
                               ;; legacy `package-tar-file-info' requires an arg
                               (package-tar-file-info file)
                             (with-no-warnings (package-tar-file-info)))))))))
    (pcase desc
      ((pred package-desc-p) desc)
      ((pred vectorp) (package-desc-from-legacy desc kind)))))

(defun quelpa-archive-file-name (archive-entry)
  "Return the path of the file in which the package for ARCHIVE-ENTRY is stored."
  (let* ((name (car archive-entry))
         (pkg-info (cdr archive-entry))
         (version (package-version-join (aref pkg-info 0)))
         (flavour (aref pkg-info 3)))
    (expand-file-name
     (format "%s-%s.%s" name version (if (eq flavour 'single) "el" "tar"))
     quelpa-packages-dir)))

(defun quelpa-build-package (rcp)
  "Build a package from the given recipe RCP.
Uses the `package-build' library to get the source code and build
an elpa compatible package in `quelpa-build-dir' storing it in
`quelpa-packages-dir'.
Return the path to the created file."
  (ignore-errors (delete-directory quelpa-build-dir t))
  (let* ((name (car rcp))
         (version (package-build-checkout name (cdr rcp) quelpa-build-dir)))
    (quelpa-archive-file-name
     (package-build-package (symbol-name name)
                            version
                            (pb/config-file-list (cdr rcp))
                            quelpa-build-dir
                            quelpa-packages-dir))))

;; --- helpers ---------------------------------------------------------------

(defun quelpa-checkout-melpa ()
  "Fetch or update the melpa source code from Github."
  (pb/checkout-git 'melpa
                   '(:url "git://github.com/milkypostman/melpa.git")
                   quelpa-melpa-dir))

(defun quelpa-get-melpa-recipe (name)
  "Read recipe with NAME for melpa git checkout.
Return the recipe if it exists, otherwise nil."
  (let* ((recipes-path (concat quelpa-melpa-dir "/recipes"))
         (files (directory-files recipes-path nil "^[^\.]+"))
         (file (assoc-string name files)))
    (when file
      (with-temp-buffer
        (insert-file-contents-literally (concat recipes-path "/" file))
        (read (buffer-string))))))

(defun quelpa-init ()
  "Setup what we need for quelpa."
  (unless (file-exists-p quelpa-packages-dir)
    (make-directory quelpa-packages-dir t))
  (unless quelpa-initialized-p
    (quelpa-setup-package-structs)
    (quelpa-checkout-melpa)
    (setq quelpa-initialized-p t)))

(defun quelpa-shutdown ()
  "Do things that need to be done after running quelpa."
  ;; remove the packages dir because we are done with the built pkgs
  (ignore-errors (delete-directory quelpa-packages-dir t)))

(defun quelpa-arg-pkg (arg)
  (pcase arg
    ((pred listp) (car arg))
    ((pred symbolp) arg)))

(defun quelpa-arg-rcp (arg)
  (pcase arg
    ((pred listp) arg)
    ((pred symbolp)
     (or (quelpa-get-melpa-recipe arg)
         (error "Quelpa cannot find a package named %s" arg)))))

(defun quelpa-parse-plist (plist)
  "Parse the optional PLIST argument of `quelpa'.
Recognized keywords are:

\:update

If t, `quelpa' is forced to do an update.
"
  (while plist
    (let ((key (car plist))
          (value (cadr plist)))
      (pcase key
        (:update (setq quelpa-force-update-p value))))
    (setq plist (cddr plist))))

(defun quelpa-package-install (arg)
  "Build and install package from ARG.
If the package has dependencies recursively call this function to
install them."
  (let ((pkg (quelpa-arg-pkg arg)))
    (unless (package-installed-p pkg)
      (let* ((rcp (quelpa-arg-rcp arg))
             (file (quelpa-build-package rcp))
             (pkg-desc (quelpa-get-package-desc file))
             (requires (package-desc-reqs pkg-desc)))
        (when requires
          (mapc (lambda (req)
                  (unless (equal 'emacs (car req))
                    (quelpa-package-install (car req))))
                requires))
        (package-install-file file)))))

;; --- public interface ------------------------------------------------------

;;;###autoload
(defun quelpa (arg &rest plist)
  "Build and install a package with quelpa.
ARG can be a package name (symbol) or a melpa recipe (list).
PLIST is a plist that may modify the build and/or fetch process."
  (run-hooks 'quelpa-before-hook)
  (quelpa-parse-plist plist)
  (quelpa-package-install arg)
  (run-hooks 'quelpa-after-hook))

(provide 'quelpa)

;;; quelpa.el ends here
