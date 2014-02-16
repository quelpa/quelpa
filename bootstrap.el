;;; bootstrap.el --- Bootstrap quelpa.el

;; Copyright 2014, Steckerhalter

;; Author: steckerhalter
;; URL: https://github.com/quelpa/quelpa
;; Version: 0.0.1
;; License: https://github.com/quelpa/quelpa/LICENSE

;; This file is not part of GNU Emacs.

;;; Code:

(require 'package)

;; `package' has to be initialized to install pkgs
(package-initialize)

;; if we do not have melpa's package-build, get the tar package and install it
(unless (require 'package-build nil t)
  (let* ((archive "http://melpa.milkbox.net/packages/")
         (archive-contents (with-temp-buffer
                             (url-insert-file-contents (concat archive "archive-contents"))
                             (cdr (read (current-buffer)))))
         (archive-entry (assoc 'package-build archive-contents))
         (archive-file-name (let* ((name (car archive-entry))
                                   (pkg-info (cdr archive-entry))
                                   (version (package-version-join (aref pkg-info 0)))
                                   (flavour (aref pkg-info 3)))
                              (format "%s-%s.%s" name version (if (eq flavour 'single) "el" "tar"))))
         (archive-url (concat archive archive-file-name)))
    (let ((file (expand-file-name (concat temporary-file-directory archive-file-name))))
      (url-copy-file archive-url file t)
      (package-install-file file))))

;; since we have package-build now, we can install the quelpa package
(unless (require 'quelpa nil t)
  (let ((file (expand-file-name (concat temporary-file-directory "quelpa.el"))))
    (url-copy-file "https://raw.github.com/quelpa/quelpa/master/quelpa.el" file t)
    (package-install-file file)))

;;; bootstrap.el ends here
