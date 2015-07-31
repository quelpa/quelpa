;;; quelpa-use-package.el --- quelpa handler for use-package

;; Copyright 2015, Steckerhalter

;; Author: steckerhalter
;; URL: https://github.com/quelpa/quelpa
;; Version: 0.0.1
;; Package-Requires: ((quelpa "0") (use-package "2"))
;; Keywords: package management elpa use-package

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
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

;; quelpa handler for `use-package'
;; See the quelpa README for more info:
;; https://github.com/quelpa/quelpa/blob/master/README.md

;;; Requirements:

;; `quelpa' and `use-package'

;;; Code:

(require 'cl-lib)
(require 'quelpa)
(require 'use-package)

(defvar quelpa-use-package-keyword :quelpa)

;; insert `:quelpa' keyword after `:disabled'
(defun quelpa-use-package-set-keyword ()
  (unless (member quelpa-use-package-keyword use-package-keywords)
    (setq use-package-keywords
          (let* ((pos (cl-position :disabled use-package-keywords))
                 (head (cl-subseq use-package-keywords 0 (+ 1 pos)))
                 (tail (nthcdr (+ 1 pos) use-package-keywords)))
            (append head (list quelpa-use-package-keyword) tail)))))

(defun use-package-normalize/:quelpa (name-symbol keyword args)
  (let ((arg (car args)))
    (pcase arg
      ((or `nil `t) (list name-symbol))
      ((pred symbolp) args)
      ((pred listp) (cond
                     ((listp (car arg)) arg)
                     ((string-match "^:" (symbol-name (car arg))) (cons name-symbol arg))
                     ((symbolp (car arg)) arg)
                     ((t nil))))
      (_ nil))))

(defun use-package-handler/:quelpa (name-symbol keyword args rest state)
  (let ((body (use-package-process-keywords name-symbol rest state)))
    ;; This happens at macro expansion time, not when the expanded code is
    ;; compiled or evaluated.
    (when args
      (quelpa-use-package-set-keyword)
      (apply 'quelpa args))
    body))

;; register keyword on require
(quelpa-use-package-set-keyword)

(provide 'quelpa-use-package)

;;; quelpa-use-package.el ends here
