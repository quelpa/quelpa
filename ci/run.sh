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

# ERT tests
emacs24 --batch --eval "(setq quelpa-ci-dir \"$dir\")" --load ert --load $dir/test/quelpa-test.el --funcall ert-run-tests-batch-and-exit
rm -rf ~/.emacs.d/
emacs-snapshot --batch --eval "(setq quelpa-ci-dir \"$dir\")" --load ert --load $dir/test/quelpa-test.el --funcall ert-run-tests-batch-and-exit
rm -rf ~/.emacs.d/

# For the file fetcher in `ci/.emacs`.
git clone https://github.com/emacs-helm/helm ~/emacs-packages/helm
git clone https://github.com/jwiegley/use-package ~/emacs-packages/use-package

# functional tests
emacs24 --batch --eval "(setq quelpa-ci-dir \"$dir\")" --load $dir/ci/.emacs
rm -rf ~/.emacs.d/
emacs-snapshot --batch --eval "(setq quelpa-ci-dir \"$dir\")" --load $dir/ci/.emacs
rm -rf ~/.emacs.d/
rm -rf ~/emacs-packages/
