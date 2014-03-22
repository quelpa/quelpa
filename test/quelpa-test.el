(unless (require 'quelpa nil t)
  (load (concat quelpa-ci-dir "/bootstrap.el"))
  (require 'quelpa))

(require 'cl)
(require 'ert nil t)

(defmacro quelpa-deftest (name arglist &optional docstring &rest body)
  "Add `quelpa-setup-p' as initial test to the given test body."
  (let ((args (when docstring (list name docstring) (list name))))
    `(ert-deftest ,@args ()
       (should (equal t (quelpa-setup-p)))
       ,@body)))

(quelpa-deftest
 quelpa-expand-recipe-test ()
 "Should be expanding correctly as return value and into buffer."
 (let ((package-build-rcp '(package-build :repo "milkypostman/melpa" :fetcher github :files ("package-build.el" "json-fix.el"))))
   (should
    (equal
     (quelpa-expand-recipe 'package-build)
     package-build-rcp)))
 (should
  (equal
   (with-temp-buffer
     (flet ((quelpa-interactive-candidate () 'package-build))
       (call-interactively 'quelpa-expand-recipe))
     (buffer-string))
   package-build-rcp)))

(quelpa-deftest
 quelpa-arg-rcp-test ()
 "Ensure `quelpa-arg-rcp' always returns the correct RCP format."
 (let ((quelpa-rcp '(quelpa :repo "quelpa/quelpa" :fetcher github))
       (package-build-rcp '(package-build :repo "milkypostman/melpa" :fetcher github :files ("package-build.el" "json-fix.el"))))
   (should
    (equal
     (quelpa-arg-rcp quelpa-rcp)
     quelpa-rcp))
   (should
    (equal
     (quelpa-arg-rcp 'package-build)
     package-build-rcp))
   (should
    (equal
     (quelpa-arg-rcp '(package-build))
     package-build-rcp))))
