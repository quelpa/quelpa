;;; bootstrap.el --- Bootstrap quelpa.el

;; Copyright 2014-2020, Steckerhalter

;; Author: steckerhalter
;; URL: https://github.com/quelpa/quelpa
;; Version: 0.0.2
;; License: https://github.com/quelpa/quelpa/LICENSE

;; This file is not part of GNU Emacs.

;;; Code:

(error "Error: `bootstrap.el' has been deprecated. Please replace the bootstrap code to use quelpa directly like this:

(unless (package-installed-p 'quelpa)
    (with-temp-buffer
      (url-insert-file-contents \"https://github.com/quelpa/quelpa/raw/master/quelpa.el\")
      (eval-buffer)
      (quelpa-self-upgrade)))

Thanks and have a nice Emacs day 8)")

;;; bootstrap.el ends here
