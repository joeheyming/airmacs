#!/bin/sh

emacs --batch --eval "(setq pkgs-to-install '($*))" -l emacs-pkg-install.el
