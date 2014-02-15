;;; bootstrap.el --- Bootstrap quelpa.el

;; Copyright 2014, Steckerhalter

;; Author: steckerhalter
;; URL: https://github.com/quelpa/quelpa
;; Version: 0.0.1
;; License: https://github.com/quelpa/quelpa/LICENSE

;; This file is not part of GNU Emacs.

;;; Code:

(require 'package)

;; if we do not have melpa's package-build, get the tar package and install it
(unless (require 'package-build nil t)
(let* ((archive "http://melpa.milkbox.net/packages/")
       (archive-contents (with-temp-buffer
			   (url-insert-file-contents (concat archive "archive-contents"))
			   (cdr (read (current-buffer)))))
       (archive-entry (assoc 'package-build archive-contents))
       (archive-url (let* ((name (car archive-entry))
				  (pkg-info (cdr archive-entry))
				  (version (package-version-join (aref pkg-info 0)))
				  (flavour (aref pkg-info 3)))
			      (format "%s%s-%s.%s" archive name version (if (eq flavour 'single) "el" "tar")))))
  (with-temp-buffer
    (url-insert-file-contents archive-url)
    (tar-mode)
    (package-install-from-buffer))))

;; since we have package-build now, we can install the quelpa package
(unless (require 'quelpa nil t)
  (with-temp-buffer
    (url-insert-file-contents "https://raw.github.com/quelpa/quelpa/master/quelpa.el")
    (package-install-from-buffer)))

;;; bootstrap.el ends here
