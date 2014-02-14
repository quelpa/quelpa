;;; quelpa.el --- Your personal ELPA with packages built directly from source

;; Copyright 2014, Steckerhalter

;; Author: steckerhalter
;; URL: https://github.com/steckerhalter/quelpa
;; Package-Requires: ((package-build "0") (servant "0"))
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

(require 'package-x)
(require 'package-build)

(defvar quelpa-dir (expand-file-name (concat user-emacs-directory "quelpa"))
  "Where quelpa builds and stores packages.")

(defvar quelpa-build-dir (concat quelpa-dir "/build")
  "Where quelpa builds packages.")

(defvar quelpa-target-dir (concat quelpa-dir "/target")
  "Where quelpa puts the built package.")

(defvar quelpa-packages-dir (concat quelpa-dir "/packages")
  "The quelpa package archive.")

(defun quelpa-archive-file-name (archive-entry)
  "Return the path of the file in which the package for ARCHIVE-ENTRY is stored."
  (let* ((name (car archive-entry))
         (pkg-info (cdr archive-entry))
         (version (package-version-join (aref pkg-info 0)))
         (flavour (aref pkg-info 3)))
    (expand-file-name
     (format "%s-%s.%s" name version (if (eq flavour 'single) "el" "tar"))
     quelpa-target-dir)))

(defun quelpa-refresh-contents ()
  "Refresh the package archive cache."
  (let ((archive `("quelpa" . ,quelpa-packages-dir)))
    (condition-case-unless-debug nil
        (package--download-one-archive archive "archive-contents")
      (error (message "Failed to download `%s' archive."
                      (car archive))))
    (package-read-archive-contents (car archive))))

(defun quelpa-init ()
  "Register the archive, create the packages dir."
  (add-to-list
   'package-archives
   `("quelpa" . ,quelpa-packages-dir))
  (unless (file-exists-p quelpa-packages-dir)
    (make-directory quelpa-packages-dir t)))

(defun quelpa-build-package (rcp)
  "Build a package from the given recipe RCP.
Uses the `package-build' library to get the source code and build
an elpa compatible package in `quelpa-build-dir'."
  (ignore-errors (delete-directory quelpa-target-dir t))
  (make-directory quelpa-target-dir t)
  (let* ((name (car rcp))
         (version (package-build-checkout name (cdr rcp) quelpa-build-dir)))
    (package-build-package (symbol-name name)
                           version
                           (pb/config-file-list rcp)
                           quelpa-build-dir
                           quelpa-target-dir)))

;;;###autoload
(defun quelpa (rcp)
  "Build and install a package from the given recipe RCP."
  (quelpa-init)
  (let ((package (car rcp))
        (package-archive-upload-base quelpa-packages-dir))
    (unless (package-installed-p package)
      (ignore-errors
        (package-upload-file
         (quelpa-archive-file-name
          (quelpa-build-package rcp))))
      (quelpa-refresh-contents)
      (package-install package))))

(provide 'quelpa)

;;; quelpa.el ends here
