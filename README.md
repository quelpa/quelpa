[![Home](https://framagit.org/steckerhalter/quelpa/raw/master/logo/quelpa-logo-h64.png)](https://framagit.org/steckerhalter/quelpa)

[![Build Status](https://travis-ci.org/quelpa/quelpa.svg?branch=master)](https://travis-ci.org/quelpa/quelpa)

Build and install your Emacs Lisp packages on-the-fly and directly from source.

### News

2018/06/18 - code moved to [framagit.org](https://framagit.org/steckerhalter/quelpa), **please open issues there** (this is only an MS mirror now 8)
2018/03/25 - support for various fetchers has been added again: `wiki`, `bzr`, `cvs`, `darcs`, `fossil`, `svn`.


<!-- doctoc command used to generate the index: doctoc --title='---' --maxlevel=3 README.md -->
<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
---

- [Overview](#overview)
- [Requirements](#requirements)
- [Installation](#installation)
- [Usage](#usage)
  - [Installing with a package name](#installing-with-a-package-name)
  - [Installing with a recipe](#installing-with-a-recipe)
  - [Upgrading individual packages](#upgrading-individual-packages)
  - [Upgrading all packages](#upgrading-all-packages)
  - [Stable packages](#stable-packages)
  - [Managing packages](#managing-packages)
  - [Additional fetchers](#additional-fetchers)
  - [Additional options](#additional-options)
- [MacOS instructions](#macos-instructions)
- [Windows instructions](#windows-instructions)
  - [Cygwin](#cygwin)
  - [Native](#native)
- [Why "quelpa"?](#why-quelpa)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

## Overview

`quelpa` is a tool to compile and install Emacs Lisp packages locally from local or remote source code.

Given a recipe in [MELPA's format](https://github.com/melpa/melpa#recipe-format), for example:

``` elisp
(quelpa '(hydra :repo "abo-abo/hydra" :fetcher github))
```

`quelpa` gets the package source code, builds an ELPA compatible package and installs that locally with `package.el`. The installed packages can then be managed in the usual way with `M-x list-packages`.

`quelpa` can be used in many ways, for example to manage your personal packages, testing development versions of other packages or as a helper when developing a package to test building, compiling and installing it.

To get an idea how to use it to manage your Emacs setup, take a look at the [steckemacs configuration](https://framagit.org/steckerhalter/steckemacs.el), which uses [quelpa-use-package](https://framagit.org/steckerhalter/quelpa-use-package) to integrate with `use-package`.

You can build and install packages from (fetcher names in parens): Git (`git`), Github (`github`), Bazaar (`bzr`), Mercurial (`hg`), Subversion (`svn`), CVS (`cvs`), Darcs (`darcs`), Fossil (`fossil`) and EmacsWiki (`wiki`)

## Requirements

-   Emacs 24.3.1
-   git: <http://git-scm.com/>

Every build requires the corresponding build tool. To install a `subversion` hosted package, you need to have `subversion` installed.

Note: Even if the sources of a package are fetched with a VCS tool you have installed, they might have dependencies that require a different VCS tool. Better install the most common ones before proceeding.

## Installation

**Note**: For Windows, see the [Instructions for Windows](#windows-instructions) before continuing

First `quelpa` needs to be installed. You can install `quelpa` either from MELPA (see https://github.com/melpa/melpa#usage for setup):


    M-x package-install RET quelpa RET

or if you are not using MELPA:

```cl
(package-initialize)
(if (require 'quelpa nil t)
    (quelpa-self-upgrade)
  (with-temp-buffer
    (url-insert-file-contents "https://framagit.org/steckerhalter/quelpa/raw/master/bootstrap.el")
    (eval-buffer)))
```

Evaluate this expression in your `*scratch*` buffer to bootstrap `quelpa`. Add it to your Emacs init file to make sure `quelpa` will be installed and upgraded automatically when needed.

If you don't like `quelpa` doing self-upgrades (although this is recommended), use the following snippet instead:

```cl
(package-initialize)
(unless (require 'quelpa nil t)
  (with-temp-buffer
    (url-insert-file-contents "https://framagit.org/steckerhalter/quelpa/raw/master/bootstrap.el")
    (eval-buffer)))
```

**Note**: `(package-initialize)` can be omitted if you are already running the command before the snippet in your init file.

## Usage

Cool. Now that we are all setup, enjoy the ride.

There are two ways to install packages with `quelpa`:

### Installing with a package name

Check <https://melpa.org/> for any packages you would like to install. You only need to know the name:

```cl
(quelpa 'magit)
```

Running this expression will fetch the `magit` source code from Github, build a package in the ELPA format and install it.

Interactive installation is supported as well. Just execute `M-x quelpa` and select a recipe name from MELPA.

If the package has dependencies they will be installed first.

### Installing with a recipe

You can also install packages that are not on MELPA. For this you need to provide a recipe in MELPA's format.

The recipe is a list and consist of the package name and keywords. A minimal recipe would look like this:

``` elisp
'(discover-my-major :fetcher git :url "https://framagit.org/steckerhalter/discover-my-major.git")
```

Depending on the fetcher, different keywords can or have to be supplied. So to install `discover-my-major` you would have to use:

``` elisp
(quelpa '(discover-my-major :fetcher git :url "https://framagit.org/steckerhalter/discover-my-major.git"))
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

Normally `quelpa` also upgrades itself as well here. You can disable this by setting `quelpa-self-upgrade-p` to `nil`:

```cl
(setq quelpa-self-upgrade-p nil)
```

### Stable packages

`quelpa` can be instructed to build stable packages. This means that the repository with the source code (`git` or `hg` are supported) is queried for a stable tag and if one is found that version will be built.

For more information please see [MELPA's notes on stable packages](https://github.com/melpa/melpa#stable-packages).

In `quelpa` there is a global variable where building of stable packages can be enabled, so that all packages are built stable (if available for the individual package):

```cl
(setq quelpa-stable-p t)
```

or you can set it just for one package by supplying `stable` as an argument:

```cl
(quelpa 'anzu :stable t)
```

or as part of the recipe itself:

```cl
(quelpa '(ag :repo "Wilfred/ag.el" :fetcher github :stable t))
```

The definition as part of the recipe has the highest priority and overrides the other two methods. Likewise adding it as an argument overrides the global variable. So the priority is like: recipe > argument > `quelpa-stable-p`.

Note that dev version numbers are usually higher than stable version numbers (they are using the build date as version) so if you want to install a stable version for an installed dev package you will first have to uninstall that package.

### Managing packages

Because `quelpa` installs packages using the built-in Emacs package management system, you can use its regular interface by executing `M-x list-packages` and work with your packages as you would normally do. Deleting a package does not affect the `quelpa` cache yet.

Currently `quelpa` does not remove obsolete packages after upgrades. To delete all obsolete packages from time to time use: 

-   `M-x list-packages RET`
-   press  `~` to mark all obsolete packages for deletion
-   press `x` and confirm deletion

### Additional fetchers

#### URL

Builds packages from single `.el` files. It works like this:

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

#### File

Builds single-file packages from local `.el` files:

``` elisp
(quelpa '(rainbow-mode :fetcher file :path "/path/to/rainbow-mode/rainbow-mode.el"))
```

Or multi-file packages from a local directory:

``` elisp
;; You can also use `~' in the :path and it will expand to the home directory.
(quelpa '(rainbow-mode :fetcher file :path "~/path/to/rainbow-mode/dir"))
```

Specifying a directory for the `:path` does not retain existing version numbers,
nor does it make any use of the `:version original` parameter.

### Additional options

#### Inhibit MELPA git checkout or updates on init

Upon initialization `quelpa` usually updates the MELPA git repo (stored in `quelpa-build-dir`/`package-build`) which ensures you always have the latest recipes from MELPA available. This causes as small delay and some people don't like that (presumably people that do not use or know `emacs --daemon` and `emacsclient`).

You can disable these updates by setting `quelpa-update-melpa-p` to `nil` before requiring `quelpa`:

```cl
(setq quelpa-update-melpa-p nil)
```

Or, if you don't want to use the MELPA git repo at all (e.g. if you're using `quelpa` mainly for installing packages outside of MELPA,) you can also set `quelpa-checkout-melpa-p` to `nil`:

```cl
(setq quelpa-checkout-melpa-p nil)
```

#### Modify MELPA Recipes

When installing a package, `quelpa` will install any listed dependencies via the recipe that MELPA has for it. But maybe you want to use a different recipe, or perhaps the dependency is not even on MELPA, which would cause an installation failure.

You can create a directory with recipe files and tell `quelpa` to look there for an appropriate recipe:

``` elisp
(add-to-list 'quelpa-melpa-recipe-stores "/path/to/custom/recipe/dir")
```

This way, if no recipe is found in this custom directory, it will fallback to the next directory in the list, which in this case would be the directory containing the recipes from MELPA.

The files themselves should be named after the package name, without any extension like `.el` or `.rcp`.

Alternatively, you can also specify a list of recipes instead.

## MacOS instructions

As MacOS is a BSD-based operating system, its `tar` command is different from GNU Tar.  If you're using an additional package manager, it should be possible to install GNU tar to resolve the issue.

### Homebrew

People using [homebrew](http://brew.sh/) will be able to install the package using the homebrew package of `gnu-tar`.

### MacPorts

People using [MacPorts](https://www.macports.org/) or Ports, which is similar in maby ways to the pkg system on FreeBSD, can resolve the issue by installing the `gnutar` package.

## Windows instructions

On Windows there are some caveats so the procedure to make Emacs work with `quelpa` is outlined below. You can either use the native Windows build from GNU or the Cygwin port. If you'd like to have a complete *nix environment on your Windows machine then the Cygwin version is to be preferred.

The Cygwin port is also easier to install as there are less manual steps necessary.

### Cygwin

#### Install Cygwin

Download either the 64-bit or 32-bit setup file from <http://cygwin.com/install.html>. If your Windows version is 64-bit then make sure to use the 64-bit installer to avoid some problems that only happen with the 32-bit version.

It is usually better to install Cygwin just for the current user, not system-wide:

- Open the location where you have downloaded the setup executable in Explorer
- Press shift and right-click -> open command prompt here

Run for example:

    setup-x86.exe --no-admin

Just leave the default packages selected and finish the installer

#### Install a sane package manager

Install apt-cyg from https://github.com/transcode-open/apt-cyg:

Open the Cygwin terminal and execute:

    lynx -source rawgit.com/transcode-open/apt-cyg/master/apt-cyg > apt-cyg
    install apt-cyg /bin

#### Install Emacs

Then install Emacs like this:

    apt-cyg install wget git emacs-w32 emacs-el

This will take quite a while...

##### Fix issue on 32-bit Cygwin

Using the 32-bit version of Cygwin I've got weird vfork errors and had to do a *rebase* (see http://cygwin.wikia.com/wiki/Rebaseall):

- Close the Cygwin terminal
- Start/Run: "C:\cygwin\bin\dash.exe" (adapt the path to your Cygwin install)
- Run: `/bin/rebaseall -v`
- Close dash

#### Enjoy

So then everything should work with `quelpa`. Open the Cygwin terminal, install your init file and enjoy Emacs on Windows (even `emacs --daemon` works with the Cygwin build)

### Native

#### Emacs

Download and unpack Emacs from <https://ftp.gnu.org/gnu/emacs/windows/> (for example to `c:\emacs`)

Some libraries are missing for SSL to work with Emacs so you will have to download the gnutls build from sourceforge: <http://sourceforge.net/projects/ezwinports/files/>

Copy all .dll files from the gnutls archive to the `c:\emacs\bin` folder.

Add `c:\emacs\bin` to the Windows `PATH` environment variable.

#### Git

Install Git from <https://git-scm.com/download/win>. **Make sure to choose these options in the installer**:

    Use Git from the Windows Command Prompt

#### Tar

`tar` is needed by `quelpa` to build the `elpa` packages and has to be installed additionally.

Download MinGW from <http://sourceforge.net/projects/mingw/files/latest/download> and start the installer.

When you can choose the packages that should get installed go to `All Packages` -> `MSYS Base System` and mark `msys-tar` (bin) for installation. Then apply the changes.

Now copy `tar`, `msys-1.0.dll`, `msys-regex-1.dll`, `msys-intl-8.dll`, `msys-iconv-2.dll` from `C:\MinGW\msys\1.0\bin` to `c:\emacs\bin`

Then Emacs should work with `quelpa`.

## Why "quelpa"?

The german word `Quelle` means `spring` (as in: water source) but also `source`. `source code` is translated to `Quellcode`. `ELPA` is the abbreviation for Emacs Lisp Package Archive. You get the idea.

