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

pwd=`pwd`

if ! test -e /usr/local/bin/emacs; then
    {
        >&2 echo "--- installing deps ---"
        sudo apt-get update
        sudo apt-get -y install build-essential wget git mercurial
        sudo apt-get -y build-dep emacs24
        wget https://ftp.gnu.org/gnu/emacs/emacs-26.3.tar.gz -O- | tar xz
        cd emacs-26.3
        >&2 echo "--- building emacs ---"
        ./configure &&\
            make &&\
            sudo make install
        sudo apt-get install -y -qq git
    } > /dev/null
fi

echo "--- running tests ---"

# ERT tests
emacs --batch --eval "(setq quelpa-ci-dir \"$dir\")" --load ert --load $dir/test/quelpa-test.el --funcall ert-run-tests-batch-and-exit
rm -rf ~/.emacs.d/

# For the file fetcher in `ci/.emacs`.
git clone https://github.com/emacs-helm/helm ~/emacs-packages/helm
git clone https://github.com/jwiegley/use-package ~/emacs-packages/use-package

# functional tests
emacs --batch --eval "(setq quelpa-ci-dir \"$dir\")" --load $dir/ci/.emacs
rm -rf ~/.emacs.d/
rm -rf ~/emacs-packages/
