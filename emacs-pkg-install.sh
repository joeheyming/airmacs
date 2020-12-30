#!/bin/sh

EMACS=emacs
OS="$(uname -s | awk '{print tolower($0)}')"
if [ "$OS" == "darwin" ]; then
  EMACS=/Applications/Emacs.app/Contents/MacOS/Emacs
fi

$EMACS --script emacs-pkg-install.el "$@"
