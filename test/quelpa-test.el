(require 'quelpa)

(require 'ert)

(defmacro quelpa-deftest (name arglist &rest body)
  "Define Quelpa ERT test.
Defines ERT test with `quelpa-' prepended to NAME and
`quelpa-setup-p' as a precondition to BODY.  ARGLIST is passed to
`ert-deftest', which see."
  (declare (indent 2))
  (let ((name (intern (concat "quelpa-" (symbol-name name))))
        (ert-plist (cl-loop while (keywordp (car body))
                            collect (pop body)
                            collect (pop body))))
    `(ert-deftest ,name ()
       ,@ert-plist
       (should (quelpa-setup-p))
       (cl-macrolet ((should-install (quelpa-args)
                                     (let* ((name (pcase quelpa-args
                                                    ((pred atom) quelpa-args)
                                                    (`((,name . ,_) . ,_) name)
                                                    (`(,(and name (pred atom)) . ,_) name))))
                                       `(progn
                                          (quelpa ',quelpa-args)
                                          (should (package-installed-p ',name))))))
         ,@body))))

(quelpa-deftest expand-recipe ()
  "Should be expanding correctly as return value and into buffer."
  (let ((package-build-rcp '(package-build :repo "melpa/package-build" :fetcher github)))
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

(quelpa-deftest arg-rcp ()
  "Ensure `quelpa-arg-rcp' always returns the correct RCP format."
  (let ((quelpa-rcp '(quelpa :repo "quelpa/quelpa" :fetcher github))
        (package-build-rcp '(package-build :repo "melpa/package-build" :fetcher github)))
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

(quelpa-deftest version>-p ()
  "Passed version should correctly be tested against the mocked
`package-alist' and built-in packages."
  (let ((package-alist (cond ((functionp 'package-desc-vers)
                              ;; old package-alist format
                              '((quelpa . [(20140406 1613)
                                           ((package-build
                                             (0)))
                                           "Emacs Lisp packages built directly from source"])))
                             ((version< emacs-version "26.1")
                              ;; New package-alist format, but before Emacs 26.
                              '((quelpa
                                 [cl-struct-package-desc
                                  quelpa
                                  (20140406 1613)
                                  "Emacs Lisp packages built directly from source"
                                  ((package-build (0))) nil nil "test" nil nil])))
                             (t
                              ;; Emacs 26+ records.
                              `((quelpa
                                 ,(record 'package-desc
                                          'quelpa
                                          '(20140406 1613)
                                          "Emacs Lisp packages built directly from source"
                                          '((package-build (0))) nil nil "test" nil nil)))))))
    (should-not (quelpa-version>-p 'quelpa "0"))
    (should-not (quelpa-version>-p 'quelpa "20140406.1613"))
    (should (quelpa-version>-p 'quelpa "20140406.1614"))
    (cl-letf (((symbol-function 'package-built-in-p)
               (lambda (name version) (version-list-<= version '(20140406 1613)))))
      (should-not (quelpa-version>-p 'foobar "0"))
      (should-not (quelpa-version>-p 'foobar "20140406.1613"))
      (should (quelpa-version>-p 'foobar "20140406.1614")))))

(quelpa-deftest check-hash ()
  "Make sure that old file hash is correctly compared with the new one
  and only when it has changed the new time-stamp is returned."
  (cl-letf* ((stamp-info '("20140413.90742" . "7e4c099e65d254f62e64b581c42ddeb3c487064b"))
             (hash "4935a306e358cbd0d9bd200e13ceb1e44942b323")
             ((symbol-function 'quelpa-build--read-from-file) (lambda (file) stamp-info))
             ((symbol-function 'quelpa-build--dump) (lambda (content file)))
             ((symbol-function 'quelpa-build--expand-source-file-list) 'ignore)
             ((symbol-function 'secure-hash) (lambda (&rest args) hash))
             ((symbol-function 'delete-directory) 'ignore)
             ((symbol-function 'make-directory) 'ignore)
             ((symbol-function 'copy-directory) 'ignore)
             ((symbol-function 'copy-file) 'ignore))
    (should-not (equal (quelpa-check-hash 'foobar nil "/" "baz")
                       (car stamp-info)))
    (setq hash (cdr stamp-info))
    (should (equal (quelpa-check-hash 'foobar nil "/" "baz")
                   (car stamp-info)))))

(quelpa-deftest cache ()
  "Ensure that installing a package with a different recipe will
update an existing cache item."
  (cl-letf ((quelpa-cache nil)
            ((symbol-function 'quelpa-package-install) 'ignore))
    (quelpa '(makey :fetcher github :repo "mickeynp/makey"))
    (quelpa 'makey)
    (should (equal quelpa-cache '((makey))))))

(quelpa-deftest cache-regressions ()
  (cl-letf ((quelpa-cache nil)
            ((symbol-function 'quelpa-package-install) 'ignore))
    (quelpa '(multiple-cursors :fetcher github :repo "magnars/multiple-cursors.el" :stable t))
    (should (equal quelpa-cache '((multiple-cursors :fetcher github :repo "magnars/multiple-cursors.el" :stable t))))
    (let ((quelpa-stable-p t))
      (quelpa '(multiple-cursors :fetcher github :repo "magnars/multiple-cursors.el")
              :stable nil))
    (should (equal quelpa-cache '((multiple-cursors :fetcher github :repo "magnars/multiple-cursors.el"))))))

(quelpa-deftest stable ()
  (cl-letf ((quelpa-cache nil)
            ((symbol-function 'quelpa-package-install) 'ignore))
    (quelpa '(2048-game :fetcher hg :url "https://bitbucket.org/zck/2048.el" :stable t))
    (quelpa 'elx :stable t)
    (let ((quelpa-stable-p t))
      (quelpa 'imgur))
    (should (equal (mapcar (lambda (item) (plist-get item :stable))
                           quelpa-cache)
                   '(t t t)))))

;;;; Installation tests

;; These tests test installing packages from various kinds of repos.
;; Copied from the old ci/.emacs file.

(quelpa-deftest add-recipe ()
  (should (add-to-list 'quelpa-melpa-recipe-stores
                       '((let-alist :fetcher url
                                    :url "http://git.savannah.gnu.org/cgit/emacs.git/plain/lisp/emacs-lisp/let-alist.el"
                                    :version original)))))

(quelpa-deftest github ()
  "Test installing from GitHub."
  (should-install (anaconda-mode
                   :fetcher github :repo "proofit404/anaconda-mode"
                   :files ("*.el" "*.py" "vendor/jedi/jedi"
                           ("jsonrpc" "vendor/jsonrpc/jsonrpc/*.py"))))
  (should-install ag)
  (should-install magit))

(quelpa-deftest hg ()
  (should-install 2048-game)
  ;; [2020-01-26 Sun 00:07] Nose repo is uncloneable due to TLS error,
  ;; but probably a local problem.
  (should-install nose))

(quelpa-deftest svn ()
  (should-install (confluence
                   :fetcher svn :url "https://svn.code.sf.net/p/confluence-el/code/trunk/"
                   :files ("confluence*.el" "*.dtd" "*.xsl"))))

(quelpa-deftest bzr ()
  (should-install (weblogger :fetcher bzr :url "lp:weblogger-el")))

(quelpa-deftest wiki ()
  (should-install (key-chord :fetcher wiki))
  (should-install (buffer-move :fetcher wiki))
  (should-install move-text))

(quelpa-deftest url ()
  ;; FIXME: For some reason this test seems to fail in batch mode but works interactively.
  :expected-result :failed
  (should-install (ox-rss
                   :url "https://code.orgmode.org/bzg/org-mode/raw/master/contrib/lisp/ox-rss.el"
                   :fetcher url))
  (should-install (rainbow-mode
                   :url "http://git.savannah.gnu.org/cgit/emacs/elpa.git/plain/packages/rainbow-mode/rainbow-mode.el"
                   :fetcher url)))

(quelpa-deftest file ()
  ;; FIXME: Helm isn't checked out to this path, of course.
  :expected-result :failed
  (should-install (helm
                   :fetcher file
                   :files ("*.el" "emacs-helm.sh"
                           (:exclude "helm.el" "helm-lib.el" "helm-source.el" "helm-match-plugin.el" "helm-core-pkg.el"))
                   :path "~/emacs-packages/helm")))

(quelpa-deftest stable ()
  ;; FIXME: Fails due to: (void-function quelpa-build--checkout-nil)
  :expected-result :failed
  (should-install (anzu :stable t))
  (should-install ((company :repo "company-mode/company-mode" :fetcher github)
                   :stable t))
  (should-install ((scss-mode :repo "antonj/scss-mode" :fetcher github)
                   :stable t))
  ;; Upgrade to non-stable.
  ;; TODO: Probably need to compare versions before and after.
  (should-install (anzu :upgrade t))
  (should-install ((company :repo "company-mode/company-mode" :fetcher github)
                   :upgrade t))
  (should-install ((scss-mode :repo "antonj/scss-mode" :fetcher github)
                   :upgrade t)))

;;;; Footer

(provide 'quelpa-tests)
