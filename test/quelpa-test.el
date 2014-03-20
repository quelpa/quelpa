(unless (require 'quelpa nil t)
  (load (concat quelpa-ci-dir "/bootstrap.el")))

(require 'ert nil t)

(eval-when-compile
  (require 'cl)
  (unless (featurep 'ert)
    (defmacro* ert-deftest (name () &body docstring-keys-and-body)
      (message "Skipping tests, ERT is not available"))))

(ert-deftest quelpa-setup-p-test ()
    (should (equal t (quelpa-setup-p))))

(ert-deftest quelpa-arg-rcp-test ()
  (should
   (equal
    (quelpa-arg-rcp '(quelpa :repo "quelpa/quelpa" :fetcher github))
    '(quelpa :repo "quelpa/quelpa" :fetcher github)))
  (should
   (equal
    (quelpa-arg-rcp 'package-build)
    '(package-build :repo "milkypostman/melpa" :fetcher github :files ("package-build.el" "json-fix.el"))))
  (should
   (equal
    (quelpa-arg-rcp '(package-build))
    '(package-build :repo "milkypostman/melpa" :fetcher github :files ("package-build.el" "json-fix.el")))))
