
install: $(HOME)/.emacs $(patsubst .elisp/%,$(HOME)/.elisp/%,$(wildcard .elisp/*))

$(HOME)/.emacs: .emacs
	cp .emacs $(HOME)/.emacs

$(HOME)/.elisp:
	-mkdir $(HOME)/.elisp

$(HOME)/.elisp/%: .elisp/% $(HOME)/.elisp
	cp -r $< $@ 
