(unless (require 'quelpa nil t)
  (load (concat quelpa-ci-dir "/bootstrap.el"))
  (require 'quelpa))

(require 'ert nil t)

(defmacro quelpa-deftest (name arglist &optional docstring &rest body)
  "Add `quelpa-setup-p' as initial test to the given test body."
  (let ((args (when docstring (list name docstring) (list name))))
    `(ert-deftest ,@args ()
       (should (equal t (quelpa-setup-p)))
       ,@body)))

(quelpa-deftest
 quelpa-exand-recipe-test ()
 "Should be expanding correctly as return value and into buffer."
 ;; TODO add test for interactive call checking output in buffer
 (should
  (equal
   (quelpa-expand-recipe 'package-build)
   '(package-build :repo "milkypostman/melpa" :fetcher github :files ("package-build.el" "json-fix.el")))))

(quelpa-deftest
 quelpa-arg-rcp-test ()
 "Ensure `quelpa-arg-rcp' always returns the correct RCP format."
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
