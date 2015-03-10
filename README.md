[![Home](https://raw.github.com/quelpa/quelpa/master/logo/quelpa-logo-h128.png)](https://github.com/quelpa/quelpa)

[![Build Status](https://travis-ci.org/quelpa/quelpa.svg?branch=master)](https://travis-ci.org/quelpa/quelpa)

Build and install your Emacs Lisp packages on-the-fly and directly from source.

If you want to help out with the development of quelpa, check out the [issues](https://github.com/quelpa/quelpa/issues).

## Overview

`quelpa` uses recipes in [MELPA's format](https://github.com/milkypostman/melpa#recipe-format) to build your desired packages from source and installs them using the built-in Emacs package manager. Basically, it's a standards-complying [el-get](https://github.com/dimitri/el-get). Or a local MELPA that doesn't require waiting for new builds.

`quelpa` can be used in many ways, for example to manage your personal packages with less limitations than a package archive would impose on you, testing development versions of other packages or as a helper when developing a package.

To get an idea how to use it to manage your Emacs setup, take a look at the [steckemacs configuration](https://github.com/steckerhalter/steckemacs.el/blob/master/steckemacs.el), where `quelpa` loads and installs the required packages just before they are configured.

You can build and install packages from all the sources MELPA's build script `package-build` supports: Git, Github, Bazaar (bzr), Mercurial (hg), Subversion (svn), CVS, Darcs, Emacs Wiki (wiki)

## Requirements

-   Emacs 24.3.1
-   git: <http://git-scm.com/>

Every build requires the corresponding build tool. To install a `subversion` hosted package, you need to have `subversion` installed.

Note: Even if the sources of a package are fetched with a VCS tool you have installed, they might have dependencies that require a different VCS tool. Better install the most common ones before proceeding.

## Installation

First `quelpa` needs to be bootstrapped. This means MELPA's `package-build` has to be installed first, then `quelpa` itself.

To bootstrap `quelpa` use this code snippet:

```cl
(package-initialize)
(if (require 'quelpa nil t)
    (quelpa-self-upgrade)
  (with-temp-buffer
    (url-insert-file-contents "https://raw.github.com/quelpa/quelpa/master/bootstrap.el")
    (eval-buffer)))
```

Evaluate this expression in your `*scratch*` buffer to bootstrap `quelpa`. Add it to your Emacs init file to make sure `quelpa` will be installed and upgraded automatically when needed.

If you don't like `quelpa` doing self-upgrades (although this is recommended), use the following snippet instead:

```cl
(package-initialize)
(unless (require 'quelpa nil t)
  (with-temp-buffer
    (url-insert-file-contents "https://raw.github.com/quelpa/quelpa/master/bootstrap.el")
    (eval-buffer)))
```

**Note**: `(package-initialize)` can be omitted if you are already running the command before the snippet in your init file.

## Usage

Cool. Now that we are all setup, enjoy the ride.

There are two ways to install packages with `quelpa`:

### Installing with a package name

Check <http://melpa.milkbox.net/> for any packages you would like to install. You only need to know the name:

```cl
(quelpa 'magit)
```

Running this expression will fetch the `magit` source code from Github, build a package in the ELPA format and install it.

Interactive installation is supported as well. Just execute `M-x quelpa` and select a recipe name from MELPA.

If the package has dependencies they will be installed first.

### Installing with a recipe

You can also install packages that are not on MELPA. For this you need to provide a recipe in MELPA's format.

For example if I'd like to install the [eval-sexp-fu.el package](http://www.emacswiki.org/emacs/eval-sexp-fu.el) which is located on the Emacs Wiki but not available on MELPA, I just need to provide a valid recipe instead of a package name:

```cl
(quelpa '(eval-sexp-fu :fetcher wiki :files ("eval-sexp-fu.el")))
```

Don't forget the quote before the recipe.

### Upgrading individual packages

Per default `quelpa` does not do anything if a package is already installed. You can customize this behavior globally by setting the variable `quelpa-upgrade-p` to `t` manually:

```cl
(setq quelpa-upgrade-p t)
```

Alternatively customize this variable by executing `M-x customize-variable quelpa-upgrade-p RET`.

It is also possible to override this default behavior for individual packages:

#### Interactive Overriding

When `quelpa` is called interactively with a prefix argument (e.g `C-u M-x quelpa`) it will try to upgrade the given package even if the global variable `quelpa-upgrade-p` is set to nil.

That means `C-u M-x quelpa magit RET` will upgrade magit.

Please note that the `:upgrade` parameter described below is still preferred over the prefix argument.

#### Non-Interactive Overriding

```cl
(quelpa 'company :upgrade t)
```

This way `quelpa` will try to upgrade `company` even if upgrading is disabled globally.

```cl
(quelpa '(ag :repo "Wilfred/ag.el" :fetcher github) :upgrade nil)
```

When used that way, `quelpa` will not upgrade `ag`. This can be used to "pin" packages when evaluating a buffer with `quelpa` invocations.

### Upgrading all packages

Upgrading all your `quelpa` packages at init is one option to keep them up to date, but can slow it down considerably. Alternatively you can execute `M-x quelpa-upgrade` and upgrade every cached package.

This command relies on an intact cache file which is set in the `quelpa-cache-file` variable. It is updated after every `quelpa` invocation. To reset it for debugging purposes, just delete the file and better keep a backup.

### Managing packages

Because `quelpa` installs packages using the built-in Emacs package management system, you can use its regular interface by executing `M-x list-packages` and work with your packages as you would normally do. Deleting a package does not affect the `quelpa` cache yet.

Currently `quelpa` does not remove obsolete packages after upgrades. To delete all obsolete packages from time to time use: 

-   `M-x list-packages RET`
-   press  `~` to mark all obsolete packages for deletion
-   press `x` and confirm deletion

### Additional fetchers

One fetcher has been added to build packages from single `.el` files. It works like this:

```cl
(quelpa '(rainbow-mode :url "http://git.savannah.gnu.org/cgit/emacs/elpa.git/plain/packages/rainbow-mode/rainbow-mode.el" :fetcher url))
```

You specify the `:url` (either a remote or local one like `file:///path/to/file.el`) of the file and as `:fetcher` `url`.

Another example:


```cl
(quelpa '(ox-rss :url "http://orgmode.org/cgit.cgi/org-mode.git/plain/contrib/lisp/ox-rss.el" :fetcher url))

```

By default upgrades are managed through file hashes, so if the content changed, `quelpa` will upgrade the package. Existing version numbers are retained. `quelpa` uses a version suffix that still allows the original version to have priority. So if you should install a package from another source with the same version it will be preferred.

To keep the original version unmodified use the parameter `:version original`. For example:

```cl
(quelpa '(queue :url "http://www.dr-qubit.org/download.php?file=predictive/queue.el" :fetcher url :version original))
```

### Additional options
#### Inhibit MELPA updates on init

Upon initialization `quelpa` usually updates the MELPA git repo (stored in `quelpa-build-dir`/`package-build`) which ensures you always have the latest recipes from MELPA available. This causes as small delay and some people don't like that (presumably people that do not use or know `emacs --daemon` and `emacsclient`).

You can disable these updates by setting `quelpa-update-melpa-p` to `nil` before requiring `quelpa`:

```cl
(setq quelpa-update-melpa-p nil)
```

## Why "quelpa"?

The german word `Quelle` means `spring` (as in: water source) but also `source`. `source code` is translated to `Quellcode`. `ELPA` is the abbreviation for Emacs Lisp Package Archive. You get the idea.
