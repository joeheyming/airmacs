
install: $(HOME)/.emacs $(patsubst .elisp/%,$(HOME)/.elisp/%,$(wildcard .elisp/*)) external pkg-install 

$(HOME)/.emacs: .emacs
	cp .emacs $(HOME)/.emacs

$(HOME)/.elisp:
	-mkdir $(HOME)/.elisp

$(HOME)/.elisp/%: .elisp/% $(HOME)/.elisp
	-/bin/cp -rf $< $@ 

# check package-install before you add to the external target
pkg-install:
	./emacs-pkg-install.sh js2-mode js2-refactor auto-complete tern tern-auto-complete load-dir nlinum python-mode hide-lines web-mode auto-save-buffers-enhanced helm helm-git-files

external:
# not github
	cd ~/.elisp; wget -Nq http://www.cb1.com/~john/computing/emacs/lisp/basics/load-directory.el 
	cd ~/.elisp; wget -Nq http://www.emacswiki.org/emacs/download/highlight-beyond-fill-column.el
