(message "You are running airmacs")

;; Are we running XEmacs or Emacs?
(defvar running-xemacs (string-match "XEmacs\\|Lucid" emacs-version))
(add-to-list 'load-path "~/.elisp")

(require 'utils)

(setq js2-mirror-mode t)
(require 'web-mode)


(setq mac-option-modifier 'super)
(setq mac-command-modifier 'meta)
(setq kill-whole-line t)

(global-set-key [C-backspace] 'util-backward-kill-word)
(global-set-key [kp-home]  'util-goto-beg)
(global-set-key [home]     'util-goto-beg)
(global-set-key [kp-end]   'util-goto-end)
(global-set-key [end]      'util-goto-end)
(global-set-key [f3] '(lambda () (interactive) (set-mark-command t)))
(global-set-key [(shift f3)] 'pop-global-mark)
(global-set-key [f7] 'mdi-maximize-restore-toggle)
(global-set-key [f8] 'util-kill-this-buffer)
(global-set-key [f10] 'comment-region)
(global-set-key [(shift f10)] 'uncomment-region)
(global-set-key [f11] 'other-window)
(global-set-key "\C-k" 'util-kill-line-or-region)
(global-set-key [s-up]
  '(lambda () (interactive) (copy-from-above-or-below 1)))
(global-set-key [C-s-up]
  '(lambda () (interactive) (copy-from-above-or-below)))
(global-set-key [s-down]
  '(lambda () (interactive) (copy-from-above-or-below 1 1)))
(global-set-key [C-s-down]
  '(lambda () (interactive) (copy-from-above-or-below 'nil 1)))
(global-set-key (kbd "C-'")    'util-toggle-kbd-macro-recording)
(global-set-key [(meta \')] 'call-last-kbd-macro)


;; iswitchb
(iswitchb-mode)
(add-hook 'iswitchb-define-mode-map-hook 'iswitchb-util-keys)
(defun iswitchb-util-keys ()
 "Add my keybindings for iswitchb."
 (define-key iswitchb-mode-map " " 'iswitchb-next-match)
 (define-key iswitchb-mode-map "\C-f" 'iswitchb-find-file)
 (define-key iswitchb-mode-map "\C-j" 'iswitchb-exit-minibuffer))

(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))

;; setup arrow keys
(global-set-key [up] 'previous-line)
(global-set-key [down] 'next-line)
(global-set-key [left] 'backward-char)
(global-set-key [right] 'forward-char)
(global-set-key [insert] 'nil)
(global-set-key "\C-a" 'util-beginning-or-toindent)
(global-set-key "\C-e" 'util-ending-or-nextline-end)

(global-set-key [C-prior] 'util-jump-to-top)
(global-set-key [C-next] 'util-jump-to-bottom)
(global-set-key [C-up] '(lambda () (interactive) (previous-line 5)))
(global-set-key [C-down] '(lambda () (interactive) (next-line 5)))
(global-set-key [(ctrl shift p)] '(lambda () (interactive) (previous-line 5)))
(global-set-key [(ctrl shift n)] '(lambda () (interactive) (next-line 5)))
(global-set-key [C-left] 'util-backward-word)
(global-set-key [C-right] 'util-forward-word)
(global-set-key [M-up] 'util-goto-matching-char)
(global-set-key [M-down] 'util-goto-matching-char)
(global-set-key [M-left] 'back-to-indentation)
(global-set-key [M-right] 'end-of-line-ignore-whitespace)
;(global-set-key [s-left] 'util-diff-left)
;(global-set-key [(super a) ?,] 'util-diff-left)
;(global-set-key [C-s-left] 'util-load-latest-diff-buffer)
;(global-set-key [s-right] 'util-diff-back-to-source)
;(global-set-key [(super a) ?.] 'util-rdiff-forward)
(global-set-key [s-up]
  '(lambda () (interactive) (copy-from-above-or-below 1)))
(global-set-key [C-s-up]
  '(lambda () (interactive) (copy-from-above-or-below)))
(global-set-key [s-down]
  '(lambda () (interactive) (copy-from-above-or-below 1 1)))
(global-set-key [C-s-down]
  '(lambda () (interactive) (copy-from-above-or-below 'nil 1)))
(global-set-key [M-s-up] 'util-scootch-up)
(global-set-key [M-s-down] 'util-scootch-down)
(global-set-key [M-s-left] 'util-scootch-left)
(global-set-key [M-s-right] 'util-scootch-right)
(global-set-key [M-S-up] 'util-scootch-up)
(global-set-key [M-S-down] 'util-scootch-down)
(global-set-key [M-S-left] 'util-scootch-left)
(global-set-key [M-S-right] 'util-scootch-right)
(global-set-key (kbd "s-0") 'util-blink-matching-char)
(global-set-key (kbd "C-s-0") 'util-goto-matching-char)
