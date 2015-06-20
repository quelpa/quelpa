(unless (require 'quelpa nil t)
  (load (concat quelpa-ci-dir "/bootstrap.el"))
  (require 'quelpa))

(require 'ert nil t)

(defmacro quelpa-deftest (name arglist docstring &rest body)
  "Add `quelpa-setup-p' as initial test to the given test body."
  (declare (doc-string 3) (indent 2))
  `(ert-deftest ,name ()
     (should (equal t (quelpa-setup-p)))
     ,@body))

(quelpa-deftest quelpa-expand-recipe-test ()
  "Should be expanding correctly as return value and into buffer."
  (let ((package-build-rcp '(package-build :repo "milkypostman/melpa" :fetcher github :files ("package-build.el"))))
    (should
     (equal
      (quelpa-expand-recipe 'package-build)
      package-build-rcp))
    (should
     (equal
      (with-temp-buffer
        (cl-letf (((symbol-function 'quelpa-interactive-candidate)
                   (lambda ()
                     (interactive)
                     'package-build)))
          (call-interactively 'quelpa-expand-recipe))
        (buffer-string))
      (prin1-to-string package-build-rcp)))))

(quelpa-deftest quelpa-arg-rcp-test ()
  "Ensure `quelpa-arg-rcp' always returns the correct RCP format."
  (let ((quelpa-rcp '(quelpa :repo "quelpa/quelpa" :fetcher github))
        (package-build-rcp '(package-build :repo "milkypostman/melpa" :fetcher github :files ("package-build.el"))))
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

(quelpa-deftest quelpa-version>-p-test ()
  "Passed version should correctly be tested against the mocked
`package-alist' and built-in packages."
  (let ((package-alist (if (functionp 'package-desc-vers)
                           ;; old package-alist format
                           '((quelpa . [(20140406 1613)
                                        ((package-build
                                          (0)))
                                        "Emacs Lisp packages built directly from source"]))
                         ;; new package-alist format
                         '((quelpa
                            [cl-struct-package-desc
                             quelpa
                             (20140406 1613)
                             "Emacs Lisp packages built directly from source"
                             ((package-build (0))) nil nil "test" nil nil])))))
    (should-not (quelpa-version>-p 'quelpa "0"))
    (should-not (quelpa-version>-p 'quelpa "20140406.1613"))
    (should (quelpa-version>-p 'quelpa "20140406.1614"))
    (cl-letf (((symbol-function 'package-built-in-p)
               (lambda (name version) (version-list-<= version '(20140406 1613)))))
      (should-not (quelpa-version>-p 'foobar "0"))
      (should-not (quelpa-version>-p 'foobar "20140406.1613"))
      (should (quelpa-version>-p 'foobar "20140406.1614")))))

(quelpa-deftest quelpa-check-file-hash-test ()
  "Make sure that old file hash is correctly compared with the new one
  and only when it has changed the new stamp-info is returned."
  (cl-letf* ((stamp-info '("20140413.907" . "7e4c099e65d254f62e64b581c42ddeb3c487064b"))
             (hash "4935a306e358cbd0d9bd200e13ceb1e44942b323")
             ((symbol-function 'package-build--read-from-file) (lambda (file) stamp-info))
             ((symbol-function 'package-build--dump) (lambda (content file)))
             ((symbol-function 'secure-hash) (lambda (&rest args) hash)))
    (should-not (equal (quelpa-check-file-hash "foobar") stamp-info))
    (setq hash (cdr stamp-info))
    (should (equal (quelpa-check-file-hash "foobar") stamp-info))))

(quelpa-deftest quelpa-cache-test ()
  "Ensure that installing a package with a different recipe will
update an existing cache item."
  (cl-letf ((quelpa-cache nil)
            ((symbol-function 'quelpa-package-install) 'ignore))
    (quelpa '(makey :fetcher github :repo "mickeynp/makey"))
    (quelpa 'makey)
    (should (equal quelpa-cache '((makey))))))

(quelpa-deftest quelpa-stable-test ()
  (cl-letf ((quelpa-cache nil)
            ((symbol-function 'quelpa-package-install) 'ignore))
    (quelpa '(2048-game :fetcher hg :url "https://bitbucket.org/zck/2048.el" :stable t))
    (quelpa 'elx :stable t)
    (let ((quelpa-stable-p t))
      (quelpa 'imgur))
    (should (equal (mapcar (lambda (item) (plist-get item :stable))
                           quelpa-cache)
                   '(t t t)))))
