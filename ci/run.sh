#!/bin/bash -e

export LANGUAGE=en_US.UTF-8
export LANG=en_US.UTF-8
export LC_ALL=en_US.UTF-8

trap "rm -rf ~/.emacs.d/" EXIT

if [ "$USER" == "vagrant" ]; then
    dir=/vagrant
else
    dir="$PWD"
fi

if ! dpkg -l | grep python-software-properties; then
    sudo apt-get update
    sudo apt-get install -qq python-software-properties
fi

if ! grep cassou /etc/apt/sources.list.d/*; then
    sudo add-apt-repository -y ppa:cassou/emacs
    sudo apt-get update
fi

if ! dpkg -l | grep emacs24; then
    sudo apt-get install -qq git mercurial subversion bzr cvs emacs24 emacs24-el emacs24-common-non-dfsg emacs-snapshot-el emacs-snapshot-gtk emacs-snapshot
fi

# ERT tests
emacs24 --batch --eval "(setq quelpa-ci-dir \"$dir\")" --load ert --load $dir/test/quelpa-test.el --funcall ert-run-tests-batch-and-exit
rm -rf ~/.emacs.d/
emacs-snapshot --batch --eval "(setq quelpa-ci-dir \"$dir\")" --load ert --load $dir/test/quelpa-test.el --funcall ert-run-tests-batch-and-exit
rm -rf ~/.emacs.d/

# functional tests
emacs24 --batch --eval "(setq quelpa-ci-dir \"$dir\")" --load $dir/ci/.emacs
rm -rf ~/.emacs.d/
emacs-snapshot --batch --eval "(setq quelpa-ci-dir \"$dir\")" --load $dir/ci/.emacs
rm -rf ~/.emacs.d/
