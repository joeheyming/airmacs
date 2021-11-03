all: external pkg-install install

install: $(patsubst .elisp/%,$(HOME)/.elisp/%,$(wildcard .elisp/*)) $(patsubst .emacs.d/snippets/%,$(HOME)/.emacs.d/snippets/%,$(wildcard .emacs.d/snippets/*))
	/bin/cp -i .emacs $(HOME)/.emacs

$(HOME)/.elisp:
	-mkdir $(HOME)/.elisp

$(HOME)/.elisp/%: .elisp/% $(HOME)/.elisp
	-/bin/cp -rf $< $@

$(HOME)/.emacs.d/snippets/:
	-mkdir -p $(HOME)/.emacs.d/snippets/

$(HOME)/.emacs.d/snippets/%: .emacs.d/snippets/% $(HOME)/.emacs.d/snippets/
	-/bin/cp -rf $< $@

PACKAGES := all-the-icons \
	auto-complete \
	csharp-mode \
	dash \
	dockerfile-mode \
	emojify \
	eslintd-fix \
	expand-region \
	f \
	flycheck \
	groovy-mode \
	helm \
	helm-git-grep \
	helm-ls-git \
	hide-lines \
	ht \
	js2-mode \
	js2-refactor \
	json-mode \
	less-css-mode \
	load-dir \
	magit \
	markdown-mode \
	multiple-cursors \
	neotree \
	nlinum \
	org \
	org-bullets \
	python-mode \
	request \
	saveplace \
	sublime-themes \
	tern \
	tern-auto-complete \
	tide \
	typescript-mode \
	web-mode \
	yasnippet \
	yaml-mode

# check package-install before you add to the external target
pkg-install:
	./emacs-pkg-install.sh $(PACKAGES)

~/.elisp/load-directory.el:
	cd ~/.elisp; wget -Nq https://raw.githubusercontent.com/atog/emacs/master/load-directory.el

~/.elisp/highlight-beyond-fill-column.el:
	cd ~/.elisp; wget -Nq https://raw.githubusercontent.com/jml/emacs-configuration/main/plugins/highlight-beyond-fill-column.el

~/.elisp/autopair.el:
	cd ~/.elisp; wget -Nq https://raw.githubusercontent.com/joaotavora/autopair/master/autopair.el


~/.elisp/compile-eslint.el:
	cd ~/.elisp; wget -Nq https://raw.githubusercontent.com/Fuco1/compile-eslint/master/compile-eslint.el

# non-github links
external: ~/.elisp/load-directory.el ~/.elisp/highlight-beyond-fill-column.el ~/.elisp/compile-eslint.el ~/.elisp/autopair.el

