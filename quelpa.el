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

(require 'servant)
(require 'package-build)

(defvar quelpa-dir (expand-file-name (concat user-emacs-directory "quelpa"))
  "Where quelpa builds and stores packages.")

(defvar quelpa-build-dir (concat quelpa-dir "/build")
  "Where quelpa builds packages.")

(defvar quelpa-packages-dir (concat quelpa-dir "/packages")
  "Where quelpa stores packages.")

(defvar quelpa-host "127.0.0.1"
  "The interface/host to bind the elpa web server to.")

(defvar quelpa-port 63336
  "The port to bind the elpa web server to.")

(defvar quelpa-server-socket nil)

(defun quelpa-server-p ()
  "Return non-nil if the server is running."
  (not
    (condition-case nil
        (delete-process
         (make-network-process
          :name "quelpa-server-started-p"
          :host quelpa-host
          :service quelpa-port))
      (error t))))

(defun quelpa-server-start ()
  "Start the elpa server unless already running."
  (unless (quelpa-server-p)
    (setq quelpa-server-socket
          (elnode-start
           (servant-make-elnode-handler quelpa-packages-dir)
           :port quelpa-port :host quelpa-host))))

(defun quelpa-build-package (rcp)
  "Build a package from the given recipe RCP.
Uses the `package-build' library to get the source code and build
an elpa compatible package in `quelpa-packages-dir'."
  (let* ((name (car rcp))
         (version (package-build-checkout name (cdr rcp) quelpa-build-dir)))
    (package-build-package (symbol-name name)
                           version
                           (pb/config-file-list rcp)
                           quelpa-build-dir
                           quelpa-packages-dir)))

(defun quelpa-init ()
  "Setup dirs, start the elpa server and so on."
  (add-to-list
   'package-archives
   `("quelpa" . ,(format "http://%s:%s/" quelpa-host quelpa-port)))
  (unless (file-exists-p quelpa-packages-dir)
    (make-directory quelpa-packages-dir t))
  (quelpa-server-start))

;;;###autoload
(defun quelpa (rcp)
  "Build and install a package from the given recipe RCP."
  (quelpa-init)
  (let ((package (car rcp)))
    (unless (package-installed-p package)
      (quelpa-build-package rcp)
      (package-refresh-contents)
      (package-install package))))

(provide 'quelpa)

;;; quelpa.el ends here
