;;; bootstrap.el --- Bootstrap quelpa.el

;; Copyright 2014, Steckerhalter

;; Author: steckerhalter
;; URL: https://github.com/quelpa/quelpa
;; Version: 0.0.1
;; License: https://github.com/quelpa/quelpa/LICENSE

;; This file is not part of GNU Emacs.

;;; Code:

(require 'package)

(defvar quelpa-ci-dir nil
  "If non-nil, quelpa will not be loaded from the url but from the given dir.")

;; `package' has to be initialized to install pkgs
(package-initialize)

(let ((temp-dir (make-temp-file "quelpa" t)))

  ;; if we do not have melpa's package-build, get the tar package and install it
  (unless (require 'package-build nil t)
    (let* ((archive "https://melpa.org/packages/")
           (archive-contents (with-temp-buffer
                               (url-insert-file-contents (concat archive "archive-contents"))
                               (cdr (read (current-buffer)))))
           (archive-entry (assoc 'package-build archive-contents))
           (archive-file-name (let* ((name (car archive-entry))
                                     (pkg-info (cdr archive-entry))
                                     (version (package-version-join (aref pkg-info 0)))
                                     (flavour (aref pkg-info 3)))
                                (format "%s-%s.%s" name version (if (eq flavour 'single) "el" "tar"))))
           (archive-url (concat archive archive-file-name))
           (file (expand-file-name archive-file-name temp-dir)))
      (url-copy-file archive-url file t)
      (package-install-file file)))

  ;; since we have package-build now, we can install the quelpa package
  (unless (require 'quelpa nil t)
    (let ((file (or (when quelpa-ci-dir (concat quelpa-ci-dir "/quelpa.el"))
                    (expand-file-name "quelpa.el" temp-dir))))
      (unless quelpa-ci-dir
        (url-copy-file "https://raw.github.com/quelpa/quelpa/master/quelpa.el" file t))
      (package-install-file file)))

  (delete-directory temp-dir t))

;;; bootstrap.el ends here
