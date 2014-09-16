(message "You are running airmacs")

;; Are we running XEmacs or Emacs?
(defvar running-xemacs (string-match "XEmacs\\|Lucid" emacs-version))
(add-to-list 'load-path "~/.elisp")

(require 'utils)
(require 'load-directory)
(load-directory "~/.elisp")

(setq default-directory "~/")

;; hide menu bar
(require 'menu-bar)
(menu-bar-mode -1)
(tool-bar-mode -1)

;; make apropros and super-apropos search through more stuff
(setq apropos-do-all t)

;; none of this truncating lines stuff
(setq truncate-partial-width-windows nil)

(setq line-move-visual nil)

;; start server for emacsclient
(server-start)

;; Use UTF8
(set-language-environment "UTF-8")

;; search for word with [f2], make backspace and delete work too
(define-key isearch-mode-map [delete] 'isearch-delete-char)
(define-key isearch-mode-map [backspace] 'isearch-delete-char)
(define-key isearch-mode-map (kbd "C-o") 'isearch-occur)
(setq tinysearch-:wrap-flag t)

;; No TABS
(setq-default indent-tabs-mode nil)
(setq-default indent-level 2)
(setq-default c-indent-level 2)
(setq-default css-indent-offset 2)

(setq confirm-kill-emacs 'yes-or-no-p)

;; Visual feedback on selections
(setq-default transient-mark-mode t)

; 'y' for 'yes', 'n' for 'no'
(fset 'yes-or-no-p 'y-or-n-p)

; Show line and column numbers in modeline
(line-number-mode t)
(column-number-mode t)

;; Enable wheelmouse support by default
(cond (window-system (mwheel-install)))
(setq mouse-wheel-progressive-speed nil)


;; Disable mouse over highlighting
(setq mouse-highlight nil)

(setq-default visible-bell t)
(blink-cursor-mode -1)
(setq blink-matching-delay 0.1)

(setq js2-mirror-mode t)
(require 'web-mode)

(setq mac-option-modifier 'super)
(setq mac-command-modifier 'meta)
(setq kill-whole-line t)

;; iswitchb
(iswitchb-mode)
(add-hook 'iswitchb-define-mode-map-hook 'iswitchb-util-keys)
(defun iswitchb-util-keys ()
 "Add my keybindings for iswitchb."
 (define-key iswitchb-mode-map " " 'iswitchb-next-match)
 (define-key iswitchb-mode-map "\C-f" 'iswitchb-find-file)
 (define-key iswitchb-mode-map "\C-j" 'iswitchb-exit-minibuffer))

(add-to-list 'auto-mode-alist '("Makefile" . makefile-mode))
(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.json\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
(add-to-list 'auto-mode-alist '("bashrc" . sh-mode))

; don't iconify on C-z when running in X
(when window-system (global-set-key "\C-z" 'util-zap-to-char))

;; Mix kbd and old-style key bindings
(define-prefix-command 'my-keymap)
(global-set-key (kbd "s-a") 'my-keymap)


(global-set-key "\C-a" 'util-beginning-or-toindent)
(global-set-key "\C-e" 'util-ending-or-nextline-end)
(global-set-key "\C-k" 'util-kill-line-or-region)
(global-set-key "\M-z"     'util-zap-to-char)
(global-set-key (kbd "C-'") 'util-toggle-kbd-macro-recording)
(global-set-key (kbd "C-s-0") 'util-goto-matching-char)
(global-set-key (kbd "s-0") 'util-blink-matching-char)
(global-set-key [(ctrl shift n)] '(lambda () (interactive) (next-line 5)))
(global-set-key [(ctrl shift p)] '(lambda () (interactive) (previous-line 5)))
(global-set-key [(meta \')] 'call-last-kbd-macro)
(global-set-key [(shift f3)] 'pop-global-mark)
(global-set-key [(shift f10)] 'uncomment-region)
(global-set-key [(super a) ?f ?d ] 'vc-diff)
(global-set-key [(super a) ?r ?f] 'util-revert-file)
(global-set-key [(super a) ?r ?h] 'util-revert-hunk)
(global-set-key [(super a) ?a ?f] 'util-apply-file)
(global-set-key [(super a) ?a ?h] 'util-apply-hunk)
(global-set-key [(super a) ?a ?i] 'util-apply-file)
(global-set-key [C-backspace] 'util-backward-kill-word)
(global-set-key [C-down] '(lambda () (interactive) (next-line 5)))
(global-set-key [C-left] 'util-backward-word)
(global-set-key [C-next] 'util-jump-to-bottom)
(global-set-key [C-prior] 'util-jump-to-top)
(global-set-key [C-right] 'util-forward-word)
(global-set-key [C-s-down] '(lambda () (interactive) (copy-from-above-or-below 'nil 1)))
(global-set-key [C-s-down] '(lambda () (interactive) (copy-from-above-or-below 'nil 1)))
(global-set-key [C-s-up] '(lambda () (interactive) (copy-from-above-or-below)))
(global-set-key [C-s-up] '(lambda () (interactive) (copy-from-above-or-below)))
(global-set-key [C-up] '(lambda () (interactive) (previous-line 5)))
(global-set-key [M-S-down] 'util-scootch-down)
(global-set-key [M-S-left] 'util-scootch-left)
(global-set-key [M-S-right] 'util-scootch-right)
(global-set-key [M-S-up] 'util-scootch-up)
(global-set-key [M-down] 'util-goto-matching-char)
(global-set-key [M-left] 'back-to-indentation)
(global-set-key [M-right] 'end-of-line-ignore-whitespace)
(global-set-key [M-s-down] 'util-scootch-down)
(global-set-key [M-s-left] 'util-scootch-left)
(global-set-key [M-s-right] 'util-scootch-right)
(global-set-key [M-s-up] 'util-scootch-up)
(global-set-key [M-up] 'util-goto-matching-char)
(global-set-key [down] 'next-line)
(global-set-key [end]      'util-goto-end)
(global-set-key [f2] 'tinysearch-search-word-forward)
(global-set-key [f3] '(lambda () (interactive) (set-mark-command t)))
(global-set-key [f5] 'run-current-file)
(global-set-key [f7] 'mdi-maximize-restore-toggle)
(global-set-key [f8] 'util-kill-this-buffer)
(global-set-key [f10] 'comment-region)
(global-set-key [f11] 'other-window)
(global-set-key [f12] 'font-lock-mode)
(global-set-key [home]     'util-goto-beg)
(global-set-key [insert] 'nil)
(global-set-key [kp-end]   'util-goto-end)
(global-set-key [kp-home]  'util-goto-beg)
(global-set-key [left] 'backward-char)
(global-set-key [right] 'forward-char)
(global-set-key [s-down] '(lambda () (interactive) (copy-from-above-or-below 1 1)))
(global-set-key [s-up] '(lambda () (interactive) (copy-from-above-or-below 1)))
(global-set-key [up] 'previous-line)

;; fix some colors
(set-face-background 'default "black")
(set-face-foreground 'default "white")
(set-mouse-color "white")
(set-cursor-color "white")

(require 'ansi-color)

; Font lock in all major modes
(global-font-lock-mode 1)
(setq font-lock-maximum-decoration t)

; hide passwords as they are entered
(add-hook 'comint-output-filter-functions
          'comint-watch-for-password-prompt)

;; Always end a file with a newline
(setq require-final-newline t)

;; Stop at the end of the file, not just add lines
(setq next-line-add-newlines nil)

(setq inhibit-startup-message    t) ; Don't want any startup message
(setq search-highlight           t) ; Highlight search object
(setq query-replace-highlight    t) ; Highlight query object
(setq mouse-sel-retain-highlight t) ; Keep mouse high-lightening


;; Case-insensitive tab completion.
(setq completion-ignore-case t)
(setq read-file-name-completion-ignore-case t)
(setq read-buffer-completion-ignore-case t)

;; smooth scrolling
(setq scroll-step 3)
(setq scroll-preserve-screen-position t)
(setq scroll-conservatively 500)
(setq next-screen-context-lines 1)
(setq automatic-hscrolling 'nil)


(require 'compile)
(setq compilation-scroll-output t)
(setq compilation-search-path
      (list "~" nil))

(setq compilation-error-regexp-alist
      (append '(
                ("# Failed test [0-9]+ in \\(.*\\) at line \\([0-9]+\\)\\( fail #[0-9]+\\)?$" 1 2)
                ("(\\([^()]*\\) at line \\([0-9]+\\)\\( fail #[0-9]+\\)?)$" 1 2)
                ("\\(~?[^ ]+\\) line \\([0-9]+\\)" 1 2)
                ("[Ll]ine \\([0-9]+\\) of \\(file:\\)?\\(/[/a-zA-Z0-9_\.\-]+\\)" 3 1)
                ;; for gjslint tests, the errors follow the ------ FILE line, nil says use the last matched file
                ("^Line \\([0-9]+\\), [EWF]" nil 1)
                ;; any detected file logs an info message
                ("\\([~a-zA-Z0-9_\.\-]*/[/a-zA-Z0-9_\.\-]*\\)[:]?\\([0-9]+\\)?" 1 2 nil 0)
                )
              (remove 'gnu compilation-error-regexp-alist))) ;gnu breaks tests with mac addrs


(add-hook 'compilation-mode-hook
          '(lambda ()
             (modify-syntax-entry ?\_ "w")
             )
          )

(setq air-dir-completions
      '(
        ("usr" "/usr")
        ("etc" "/etc")
        ("roo" "/root")
        ("hom" "~/")
        ("var" "/var")
        ("log" "/var/log")
        )
      )

(setq PC-word-delimiters "-_ ")
(define-key minibuffer-local-completion-map " " 'air-findfile-completion)
(setq minibuffer-local-filename-completion-map minibuffer-local-completion-map)
(setq minibuffer-local-must-match-filename-map minibuffer-local-must-match-map)
