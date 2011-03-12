#!/bin/bash
emacs-compile-file -eval "(add-to-list 'load-path \"$PWD/predictive\")(add-to-list 'load-path \"$PWD/ecb\")(add-to-list 'load-path \"$PWD/cedet\")(load-library \"cedet.el\")" lazycat-toolkit.el
