
install: $(HOME)/.emacs $(patsubst .elisp/%,$(HOME)/.elisp/%,$(wildcard .elisp/*)) external

$(HOME)/.emacs: .emacs
	cp .emacs $(HOME)/.emacs

$(HOME)/.elisp:
	-mkdir $(HOME)/.elisp

$(HOME)/.elisp/%: .elisp/% $(HOME)/.elisp
	-/bin/cp -rf $< $@ 

external:
# not github
	cd ~/.elisp; wget -Nq http://www.cb1.com/~john/computing/emacs/lisp/basics/load-directory.el 
	cd ~/.elisp; wget -Nq http://www.emacswiki.org/emacs-en/download/setnu.el
	cd ~/.elisp; wget -Nq http://www.emacswiki.org/emacs-de/download/hide-lines.el
	cd ~/.elisp; wget -Nq http://www.emacswiki.org/emacs/download/highlight-beyond-fill-column.el
	cd ~/.elisp; wget -Nq http://www.emacswiki.org/emacs/download/auto-save.el
#github
	curl -s --insecure https://raw.githubusercontent.com/fxbois/web-mode/master/web-mode.el -o ~/.elisp/web-mode.el
	curl -s --insecure https://raw.githubusercontent.com/mooz/js2-mode/master/js2-mode.el -o ~/.elisp/js2-mode.el
	curl -s --insecure https://raw.githubusercontent.com/emacsmirror/python-mode/master/python-mode.el -o ~/.elisp/python-mode.el
