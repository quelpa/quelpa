;; bootstrap
(unless (require 'quelpa nil t)
  (load (concat quelpa-ci-dir "/bootstrap.el")))

;; github
(quelpa '(discover-my-major :fetcher github :repo "steckerhalter/discover-my-major"))
(quelpa '(anaconda-mode :fetcher github :repo "proofit404/anaconda-mode" :files ("*.el" "*.py" "vendor/jedi/jedi" ("jsonrpc" "vendor/jsonrpc/jsonrpc/*.py"))))
(quelpa 'ag)
(when (version<= "24.4" emacs-version)
  (quelpa 'magit))

;; hg
(quelpa '2048-game)
(quelpa 'latex-pretty-symbols)
(quelpa 'nose)

;; url
(quelpa '(ox-rss :url "http://orgmode.org/cgit.cgi/org-mode.git/plain/contrib/lisp/ox-rss.el" :fetcher url))
(quelpa '(rainbow-mode :url "http://git.savannah.gnu.org/cgit/emacs/elpa.git/plain/packages/rainbow-mode/rainbow-mode.el" :fetcher url))

;; file
(quelpa '(helm :fetcher file :files ("*.el" "emacs-helm.sh" (:exclude "helm.el" "helm-lib.el" "helm-source.el" "helm-match-plugin.el" "helm-core-pkg.el")) :path "~/emacs-packages/helm"))
(quelpa '(use-package :fetcher file :path "~/emacs-packages/use-package/use-package.el"))

;; upgrade-test
(quelpa '(quelpa :repo "quelpa/quelpa" :fetcher github) :upgrade t)

;; stable packages
(quelpa 'anzu :stable t)
(quelpa '(company :repo "company-mode/company-mode" :fetcher github) :stable t)
(quelpa '(scss-mode :repo "antonj/scss-mode" :fetcher github) :stable t)

;; try upgrading the stable packages to dev
(quelpa 'anzu :upgrade t)
(quelpa '(company :repo "company-mode/company-mode" :fetcher github) :upgrade t)
(quelpa '(scss-mode :repo "antonj/scss-mode" :fetcher github) :upgrade t)
