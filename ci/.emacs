;; bootstrap
(unless (require 'quelpa nil t)
  (load (concat quelpa-ci-dir "/bootstrap.el")))

;; wiki
(quelpa '(key-chord :fetcher wiki))
(quelpa '(buffer-move :fetcher wiki))
(quelpa 'move-text)

;; github
(quelpa '(discover-my-major :fetcher github :repo "steckerhalter/discover-my-major"))
(quelpa '(anaconda-mode :fetcher github :repo "proofit404/anaconda-mode" :files ("*.el" "*.py" "vendor/jedi/jedi" ("jsonrpc" "vendor/jsonrpc/jsonrpc/*.py"))))
(quelpa 'ag)

;; hg
(quelpa '(nav :url "https://code.google.com/p/emacs-nav/" :fetcher hg :files ("ack*" "nav.el")))
(quelpa '(furl :fetcher hg :url "https://code.google.com/p/furl-el/"))
(quelpa 'nose)

;; svn
(quelpa '(confluence :fetcher svn :url "http://confluence-el.googlecode.com/svn/trunk/" :files ("confluence*.el" "*.dtd" "*.xsl")))
(quelpa '(caml :fetcher svn :url "http://caml.inria.fr/svn/ocaml/trunk/emacs/"))
(quelpa 'tuareg)

;; bzr
(quelpa '(weblogger :fetcher bzr :url "lp:weblogger-el"))
(quelpa '(xml-rpc :fetcher bzr :url "lp:xml-rpc-el"))
(quelpa 'mediawiki)

;; cvs
(quelpa '(w3m :url ":pserver:anonymous@cvs.namazu.org:/storage/cvsroot" :module "emacs-w3m" :fetcher cvs))
(quelpa 'matlab-mode)

;; url
(quelpa '(ox-rss :url "http://orgmode.org/cgit.cgi/org-mode.git/plain/contrib/lisp/ox-rss.el" :fetcher url))
(quelpa '(rainbow-mode :url "http://git.savannah.gnu.org/cgit/emacs/elpa.git/plain/packages/rainbow-mode/rainbow-mode.el" :fetcher url))

;; upgrade-test
(quelpa '(quelpa :repo "quelpa/quelpa" :fetcher github) :upgrade t)

;; stable packages
(quelpa 'anzu :stable t)
(quelpa 'magit :stable t)
(quelpa '(company :repo "company-mode/company-mode" :fetcher github) :stable t)
(quelpa '(scss-mode :repo "antonj/scss-mode" :fetcher github) :stable t)
