
install: $(HOME)/.emacs $(patsubst .elisp/%,$(HOME)/.elisp/%,$(wildcard .elisp/*)) extra

$(HOME)/.emacs: .emacs
	cp .emacs $(HOME)/.emacs

$(HOME)/.elisp:
	-mkdir $(HOME)/.elisp

$(HOME)/.elisp/%: .elisp/% $(HOME)/.elisp
	cp -r $< $@ 

~/.elisp/load-directory.el:
	wget http://www.cb1.com/~john/computing/emacs/lisp/basics/load-directory.el -O ~/.elisp
