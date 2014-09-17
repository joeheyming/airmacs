
install: $(HOME)/.emacs $(patsubst .elisp/%,$(HOME)/.elisp/%,$(wildcard .elisp/*)) external

$(HOME)/.emacs: .emacs
	cp .emacs $(HOME)/.emacs

$(HOME)/.elisp:
	-mkdir $(HOME)/.elisp

$(HOME)/.elisp/%: .elisp/% $(HOME)/.elisp
	-/bin/cp -rf $< $@ 

external:
	wget http://www.cb1.com/~john/computing/emacs/lisp/basics/load-directory.el -O ~/.elisp/load-directory.el
	wget http://www.emacswiki.org/emacs-en/download/setnu.el -O ~/.elisp/setnu.el
	wget http://www.emacswiki.org/emacs-de/download/hide-lines.el -O ~/.elisp/hide-lines.el
	curl --insecure https://raw.githubusercontent.com/fxbois/web-mode/master/web-mode.el -o ~/.elisp/web-mode.el
