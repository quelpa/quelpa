;; bootstrap
(package-initialize)
(unless (require 'quelpa nil t)
  (load (concat quelpa-ci-dir "/bootstrap.el"))
  (require 'quelpa))

;; test adding a list recipe store (let-alist will be required by magit)
(add-to-list 'quelpa-melpa-recipe-stores
             '((let-alist :fetcher url
                          :url "http://git.savannah.gnu.org/cgit/emacs.git/plain/lisp/emacs-lisp/let-alist.el"
                          :version original)))

;; github
(quelpa '(discover-my-major :fetcher github :repo "steckerhalter/discover-my-major"))
(quelpa '(anaconda-mode :fetcher github :repo "proofit404/anaconda-mode" :files ("*.el" "*.py" "vendor/jedi/jedi" ("jsonrpc" "vendor/jsonrpc/jsonrpc/*.py"))))
(quelpa 'ag)
(quelpa 'magit)

;; hg
(quelpa '2048-game)
(quelpa 'latex-pretty-symbols)
(quelpa 'nose)

;; svn
(quelpa '(confluence :fetcher svn :url "https://svn.code.sf.net/p/confluence-el/code/trunk/" :files ("confluence*.el" "*.dtd" "*.xsl")))

;; bzr
(quelpa '(weblogger :fetcher bzr :url "lp:weblogger-el"))
(quelpa '(xml-rpc :fetcher bzr :url "lp:xml-rpc-el"))
(quelpa 'color-theme)

;; wiki
(quelpa '(key-chord :fetcher wiki))
(quelpa '(buffer-move :fetcher wiki))
(quelpa 'move-text)

;; url
(quelpa '(ox-rss :url "https://code.orgmode.org/bzg/org-mode/raw/master/contrib/lisp/ox-rss.el" :fetcher url))
(quelpa '(rainbow-mode :url "http://git.savannah.gnu.org/cgit/emacs/elpa.git/plain/packages/rainbow-mode/rainbow-mode.el" :fetcher url))

;; file
(quelpa '(helm :fetcher file :files ("*.el" "emacs-helm.sh" (:exclude "helm.el" "helm-lib.el" "helm-source.el" "helm-match-plugin.el" "helm-core-pkg.el")) :path "~/emacs-packages/helm"))

;; upgrade-test
;;(quelpa '(quelpa :repo "quelpa/quelpa" :fetcher github) :upgrade t)

;; stable packages
(quelpa 'anzu :stable t)
(quelpa '(company :repo "company-mode/company-mode" :fetcher github) :stable t)
(quelpa '(scss-mode :repo "antonj/scss-mode" :fetcher github) :stable t)

;; try upgrading the stable packages to dev
(quelpa 'anzu :upgrade t)
(quelpa '(company :repo "company-mode/company-mode" :fetcher github) :upgrade t)
(quelpa '(scss-mode :repo "antonj/scss-mode" :fetcher github) :upgrade t)
