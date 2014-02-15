;;; quelpa.el --- Your personal ELPA with packages built directly from source

;; Copyright 2014, Steckerhalter

;; Author: steckerhalter
;; URL: https://github.com/quelpa/quelpa
;; Version: 0.0.1
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

(require 'package-build)

;; --- variables -------------------------------------------------------------

(defvar quelpa-dir (expand-file-name (concat user-emacs-directory "quelpa"))
  "Where quelpa builds and stores packages.")

(defvar quelpa-build-dir (concat quelpa-dir "/build")
  "Where quelpa builds packages.")

(defvar quelpa-packages-dir (concat quelpa-dir "/packages")
  "The quelpa package archive.")

;; --- archive-contents building ---------------------------------------------

(defun quelpa-package-type (file)
  "Determine the package type of FILE.
Return `tar' for tarball packages, `single' for single file
packages, or nil, if FILE is not a package."
  (let ((ext (file-name-extension file)))
    (cond
     ((string= ext "tar") 'tar)
     ((string= ext "el") 'single)
     (:else nil))))

(defun quelpa-create-index-entry (file)
  "Create a package index entry for the package at FILE.
Return a package index entry."
  (let ((pkg-desc (quelpa-get-package-desc file)))
    (when pkg-desc
      (let* ((file-type (package-desc-kind pkg-desc))
             (pkg-name (package-desc-name pkg-desc))
             (requires (package-desc-reqs pkg-desc))
             (desc (package-desc-summary pkg-desc))
             (split-version (package-desc-version pkg-desc))
             (extras (package-desc-extras pkg-desc)))
        (cons pkg-name (package-make-ac-desc split-version requires desc file-type extras))))))

(defun quelpa-get-package-desc (file)
  "Extract and return the PACKAGE-DESC struct from FILE.
On error return nil."
  (with-demoted-errors "Error getting PACKAGE-DESC: %s"
    (with-temp-buffer
      (insert-file-contents-literally file)
      (pcase (quelpa-package-type file)
        (`single (package-buffer-info))
        (`tar (tar-mode)
              (with-no-warnings (package-tar-file-info)))))))

(defun quelpa-create-index (directory)
  "Generate a package index for DIRECTORY."
  (let* ((package-files (delq nil (mapcar (lambda (f) (when (quelpa-package-type f) f))
                                          (directory-files directory t))))
         (entries (delq nil (mapcar 'quelpa-create-index-entry package-files))))
    (append (list 1) entries)))

(defun quelpa-create-index-string (directory)
  "Generate a package index for DIRECTORY as string."
  (let ((print-level nil)
        (print-length nil))
    (concat "\n" (prin1-to-string (quelpa-create-index directory)))))

(defun quelpa-build-archive-contents ()
  (with-temp-file (concat quelpa-packages-dir "/archive-contents")
    (insert (quelpa-create-index-string quelpa-packages-dir))))

;; --- package building ------------------------------------------------------

(defun quelpa-build-package (rcp)
  "Build a package from the given recipe RCP.
Uses the `package-build' library to get the source code and build
an elpa compatible package in `quelpa-build-dir'."
  (ignore-errors (delete-directory quelpa-build-dir t))
  (let* ((name (car rcp))
         (version (package-build-checkout name (cdr rcp) quelpa-build-dir)))
    (package-build-package (symbol-name name)
                           version
                           (pb/config-file-list rcp)
                           quelpa-build-dir
                           quelpa-packages-dir)))

;; --- helpers ---------------------------------------------------------------

(defun quelpa-refresh-contents ()
  "Refresh the elpa package archive cache."
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

;; --- public interface ------------------------------------------------------

;;;###autoload
(defun quelpa (rcp)
  "Build and install a package from the given recipe RCP."
  (quelpa-init)
  (let ((package (car rcp))
        (package-archive-upload-base quelpa-packages-dir))
    (unless (package-installed-p package)
      (quelpa-build-package rcp)
      (quelpa-build-archive-contents)
      (quelpa-refresh-contents)
      (package-install package))))

(provide 'quelpa)

;;; quelpa.el ends here
