;; bootstrap
(unless (require 'quelpa nil t)
  (load quelpa-bootstrap-file))

;; wiki
(quelpa '(key-chord :fetcher wiki))
(quelpa '(buffer-move :fetcher wiki))
(quelpa 'move-text)

;; github
(quelpa '(discover-my-major :fetcher github :repo "steckerhalter/discover-my-major"))
(quelpa '(grandshell-theme :repo "steckerhalter/grandshell-theme" :fetcher github))
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
(quelpa '(swbuff :fetcher cvs :url ":pserver:anonymous@emhacks.cvs.sourceforge.net:/cvsroot/emhacks" :module "emhacks/swbuff.el"))
(quelpa 'matlab-mode)
