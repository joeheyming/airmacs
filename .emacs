(message "You are running /Users/joeheyming/.emacs")

;; Are we running XEmacs or Emacs?
(defvar running-xemacs (string-match "XEmacs\\|Lucid" emacs-version))
(add-to-list 'load-path "~/.elisp")

(require 'utils)

(setq js2-mirror-mode t)
(require 'web-mode)


(setq mac-option-modifier 'super)
(setq kill-whole-line t)

(global-set-key [C-backspace] 'util-backward-kill-word)
(global-set-key [kp-home]  'util-goto-beg)
(global-set-key [home]     'util-goto-beg)
(global-set-key [kp-end]   'util-goto-end)
(global-set-key [end]      'util-goto-end)
(global-set-key [f3] '(lambda () (interactive) (set-mark-command t)))
(global-set-key [(shift f3)] 'pop-global-mark)
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
