
install: $(patsubst .elisp/%,$(HOME)/.elisp/%,$(wildcard .elisp/*)) $(patsubst .emacs.d/snippets/%,$(HOME)/.emacs.d/snippets/%,$(wildcard .emacs.d/snippets/*)) external pkg-install 
	/bin/cp -i .emacs $(HOME)/.emacs

$(HOME)/.elisp:
	-mkdir $(HOME)/.elisp

$(HOME)/.elisp/%: .elisp/% $(HOME)/.elisp
	-/bin/cp -rf $< $@ 

$(HOME)/.emacs.d/snippets/:
	-mkdir -p $(HOME)/.emacs.d/snippets/

$(HOME)/.emacs.d/snippets/%: .emacs.d/snippets/% $(HOME)/.emacs.d/snippets/
	-/bin/cp -rf $< $@ 

# check package-install before you add to the external target
pkg-install:
	./emacs-pkg-install.sh js2-mode js2-refactor auto-complete tern tern-auto-complete load-dir nlinum python-mode hide-lines web-mode auto-save-buffers-enhanced helm helm-ls-git helm-git-grep js2-refactor multiple-cursors requirejs-mode json-mode markdown-mode

external:
# non-github links
	cd ~/.elisp; wget -Nq http://www.cb1.com/~john/computing/emacs/lisp/basics/load-directory.el 
	cd ~/.elisp; wget -Nq http://www.emacswiki.org/emacs/download/highlight-beyond-fill-column.el
