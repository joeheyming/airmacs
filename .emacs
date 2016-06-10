(message "You are running airmacs")

;; Are we running XEmacs or Emacs?
(defvar running-xemacs (string-match "XEmacs\\|Lucid" emacs-version))
(add-to-list 'load-path "~/.elisp")
;; package-initialize may fail under some versions of emacs.
;; Commenting it out appears to fix that in those cases.
(package-initialize)
(setenv "PATH" (concat "/usr/local/bin:" (getenv "PATH")))

(require 'cl)
(require 'air-utils)

(setq default-directory "~/")

;; quickly jump to lisp functions
(require 'find-func)

;; hide menu bar
(require 'menu-bar)
(menu-bar-mode -1)
(tool-bar-mode -1)

(setq setnu-line-number-format "%3d")

(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward)

;; make apropros and super-apropos search through more stuff
(setq apropos-do-all t)

;; none of this truncating lines stuff
(setq truncate-partial-width-windows nil)

(setq line-move-visual nil)

;; start server for emacsclient
(server-start)

;; Use UTF8
(set-language-environment "UTF-8")

(define-key isearch-mode-map [delete] 'isearch-delete-char)
(define-key isearch-mode-map [backspace] 'isearch-delete-char)
(define-key isearch-mode-map (kbd "C-o") 'isearch-occur)

;; highlight long lines
(setq default-fill-column 115)
(defface highlight-beyond-fill-column-my-face
  '((((class color)
      (background dark))
     (:foreground "DarkSeaGreen2" :bold t))
    (((class color)
      (background light))
     (:foreground "ForestGreen" :bold t))
    (t
     ()))
  "*Face used by highlight-beyond-fill-column")
(setq highlight-beyond-fill-column-face 'highlight-beyond-fill-column-my-face)
(setq highlight-beyond-fill-column-in-modes
      '("perl-mode"
        "airwave-cperl-mode"
        "cperl-mode"))

;; Show trailing whitespace in normal buffers
(setq-default show-trailing-whitespace nil)

(setq-default auto-save-directory "~/.autosaves/")


;; No TABS
(setq-default indent-tabs-mode nil)
(setq-default indent-level 2)
(setq-default c-indent-level 2)
(setq-default css-indent-offset 2)

(require 'comint)
(setq confirm-kill-emacs 'yes-or-no-p)

;; Visual feedback on selections
(setq-default transient-mark-mode t)

; 'y' for 'yes', 'n' for 'no'
(fset 'yes-or-no-p 'y-or-n-p)


; Show line and column numbers in modeline
(column-number-mode t)

;; Enable wheelmouse support by default
(cond (window-system (mwheel-install)))
(setq mouse-wheel-progressive-speed nil)


;; Disable mouse over highlighting
(setq mouse-highlight nil)

(setq-default visible-bell t)
(setq ring-bell-function
      (lambda ()
        (invert-face 'mode-line)
        (run-with-timer 0.1 nil 'invert-face 'mode-line)))

(blink-cursor-mode -1)
(setq blink-matching-delay 0.1)

(require 'js2-mode)
(setq js2-mirror-mode t)

(require 'web-mode)
(setq web-mode-comment-style 2)
(setq standard-indent 2)
(setq web-mode-indent-style 2)
(setq-default indent-tabs-mode t)

(setq mac-option-modifier 'super)
(setq mac-command-modifier 'meta)
(setq kill-whole-line t)

(require 'yasnippet)
(add-hook 'prog-mode-hook
          '(lambda ()
             (yas-minor-mode)))

(yas/global-mode 1)
(yas/initialize)

;; Tab completion
(setq hippie-expand-try-functions-list (list
  'yas-hippie-try-expand
  'util-try-expand-hashitems
  'try-expand-dabbrev-visible
  'try-expand-dabbrev
  'try-expand-dabbrev-all-buffers
  'try-expand-dabbrev-from-kill
  'try-complete-file-name-partially
  'try-complete-file-name
))

(define-key ctl-x-map "\C-b" 'electric-buffer-list)

;; iswitchb
;(ido-mode)
(iswitchb-mode)
(add-hook 'iswitchb-define-mode-map-hook 'iswitchb-util-keys)
(defun iswitchb-util-keys ()
 "Add my keybindings for iswitchb."
 (define-key iswitchb-mode-map " " 'iswitchb-next-match)
 (define-key iswitchb-mode-map "\C-f" 'iswitchb-find-file)
 (define-key iswitchb-mode-map "\C-j" 'iswitchb-exit-minibuffer))

(autoload 'cperl-mode "cperl-mode")
(autoload 'perl-mode "perl-mode")
(autoload 'java-mode "java-mode")
(autoload 'js2-mode "js2-mode")
(autoload 'lisp-mode "lisp-mode")
(autoload 'ruby-mode "ruby-mode")
(autoload 'scss-mode "scss-mode")
(autoload 'web-mode "web-mode")

(add-to-list 'auto-mode-alist '("\\.log\\'" . auto-revert-mode))
(add-to-list 'auto-mode-alist '("Makefile" . makefile-mode))
(add-to-list 'auto-mode-alist '("\\.\\(html\\|mustache\\)" . web-mode))
(add-to-list 'auto-mode-alist '("\\.js" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.scss" . scss-mode))
(add-to-list 'auto-mode-alist '("\\.less" . css-mode))
(add-to-list 'auto-mode-alist '("bashrc" . sh-mode))
(add-to-list 'auto-mode-alist
             '("\\.\\(?:gemspec\\|irbrc\\|gemrc\\|rake\\|rb\\|ru\\|thor\\)\\'" . ruby-mode))
(add-to-list 'auto-mode-alist
             '("\\(Capfile\\|Gemfile\\(?:\\.[a-zA-Z0-9._-]+\\)?\\|[rR]akefile\\)\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.org" . org-mode))
(add-to-list 'auto-mode-alist '("\\.md" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.json" . json-mode))
(add-to-list 'auto-mode-alist '("\\.properties" . conf-javaprop-mode))

; don't iconify on C-z when running in X
(when window-system (global-set-key "\C-z" 'util-zap-to-char))

;; Mix kbd and old-style key bindings
(define-prefix-command 'my-keymap)
(global-set-key (kbd "s-a") 'my-keymap)

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
(add-hook 'compilation-filter-hook 'comint-truncate-buffer)
(setq comint-buffer-maximum-size 2000)
(setq compilation-search-path
      (list "~" nil))

(setq compilation-error-regexp-alist
      (append '(
                ("# Failed test [0-9]+ in \\(.*\\) at line \\([0-9]+\\)\\( fail #[0-9]+\\)?$" 1 2)
                ("(\\([^()]*\\) at line \\([0-9]+\\)\\( fail #[0-9]+\\)?)$" 1 2)
                ("\"\\(~?[^ ]+\\)\", line \\([0-9]+\\)" 1 2)
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

(require 'diff-mode)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(compilation-info ((((class color) (min-colors 88) (background dark)) (:foreground "lightpink" :weight bold :underline nil))))
 '(cperl-array-face ((t (:foreground "gold"))))
 '(cperl-hash-face ((t (:foreground "firebrick1"))))
 '(diff-added ((t (:foreground "dark turquoise"))))
 '(diff-file-header ((t (:foreground "firebrick" :weight bold))))
 '(diff-function ((t (:inherit diff-context-face :foreground "DarkGoldenrod1"))))
 '(diff-header ((((class color) (background dark)) (:foreground "forest green"))))
 '(diff-index ((t (:inherit diff-file-header-face :underline t))))
 '(diff-refine-change ((t (:background "gray30"))))
 '(diff-removed ((t (:foreground "violet"))))
 '(font-lock-comment-face ((t (:foreground "chocolate1"))))
 '(trailing-whitespace ((((class color) (background dark)) (:background "grey30")))))


;; setdefault window size
(setq default-frame-alist (append (list
  '(width . 95) '(height . 57)
  '(vertical-scroll-bars . right)
  '(font . "-misc-fixed-medium-r-normal--20-200-75-75-c-100-iso8859-1"))
  default-frame-alist))


(defun comment-line (comment_fn)
  (interactive)
  (save-excursion
    (if (not mark-active)
        (progn
          (set-mark (line-beginning-position))
          (end-of-line)))
    (funcall comment_fn (region-beginning) (region-end))
    (deactivate-mark))
  (next-line))

(require 'load-directory)
(load-directory "~/.elisp")

;;
;; anything config to locate
(require 'helm-config)

;;
;; find files in git project
(require 'helm-ls-git)
(require 'expand-region)
(require 'js2-refactor)
(require 'multiple-cursors)

(defun eval-region-verbose ()
  (interactive) 
  (eval-region (region-beginning) (region-end)) (deactivate-mark) 
  (message "Region Eval'd"))

(defun git-grep-word-or-region ()
  (interactive)
  (helm-git-grep-1 (util-region-or-word)))

(defun split-window-4()
 "Split window into 4 sub-window"
 (interactive)
 (if (= 1 (length (window-list)))
     (progn (split-window-vertically)
	    (split-window-horizontally)
	    (other-window 2)
	    (split-window-horizontally)
	    )))

(global-set-key [f4] 'helm-git-grep-at-point)
(global-set-key [(shift f4)] 'helm-imenu)

(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

(global-set-key (kbd "C-=") 'er/expand-region)
(global-set-key (kbd "C-c C-l") 'helm-ls-git-ls)
(global-set-key (kbd "C-x 4") 'split-window-4)
(global-set-key "\C-a" 'util-beginning-or-toindent)
(global-set-key "\C-e" 'util-ending-or-nextline-end)
(global-set-key "\C-k" 'util-kill-line-or-region)
(global-set-key "\M-\C-s"  'util-findcode)
(global-set-key "\M-z"     'util-zap-to-char)
(global-set-key (kbd "C-%") 'util-region-replace)
(global-set-key (kbd "C-'") 'util-toggle-kbd-macro-recording)
(global-set-key (kbd "C-s-%") 'util-region-replace-regex)
(global-set-key (kbd "C-s-0") 'util-goto-matching-char)
(global-set-key (kbd "s-0") 'util-blink-matching-char)
(global-set-key [(ctrl shift n)] '(lambda () (interactive) (next-line 5)))
(global-set-key [(ctrl shift p)] '(lambda () (interactive) (previous-line 5)))
(global-set-key [(meta \')] 'call-last-kbd-macro)
(global-set-key [(shift f10)] '(lambda () (interactive) (comment-line 'uncomment-region)))
(global-set-key [(shift f3)] 'pop-global-mark)
(global-set-key [(super \,)] '(lambda () (interactive) (util-ensure-trailing-thing ",")))
(global-set-key [(super \;)] '(lambda () (interactive) (util-ensure-trailing-thing ";")))
(global-set-key [(super a) ?a ?f] 'util-apply-file)
(global-set-key [(super a) ?a ?h] 'util-apply-hunk)
(global-set-key [(super a) ?a ?i] 'util-apply-file)
(global-set-key [(super a) ?c ?m ] 'chmod)
(global-set-key [(super a) ?c ?l ] 'collapse-list)
(global-set-key [(super a) ?c ?r ] 'vc-resolve-conflicts)
(global-set-key [(super a) ?d ?f ] 'delete-this-buffer-and-file)
(global-set-key [(super a) ?d ?o ] 'util-delete-other-buffers)
(global-set-key [(super a) ?e ?l ] 'eightyify-list)
(global-set-key [(super a) ?f ?d ] 'vc-diff)
(global-set-key [(super a) ?f ?g ] 'util-findgrep)
(global-set-key [(super a) ?g ?g ] 'helm-git-grep)
(global-set-key [(super a) ?g ?s ] '(lambda() (interactive) (compile (format "cd %s; git status" (vc-root-or-current-dir)))))
(global-set-key [(super a) ?k ?o ] 'util-kill-other-buffers)
(global-set-key [(super a) ?g ?s ] 'magit-status)
(global-set-key [(super a) ?m ?s ] 'magit-diff-staged)
(global-set-key [(super a) ?r ?d ] '(lambda() (interactive) (util-save-and-save-some-buffers) (vc-root-diff nil)))
(global-set-key [(super a) ?p ?x] 'util-pretty-xml)
(global-set-key [(super a) ?r ?f] 'util-revert-file)
(global-set-key [(super a) ?r ?h] 'util-revert-hunk)
(global-set-key [(super a) ?s ?a ] 'vc-annotate)
(global-set-key [(super a) ?s ?w ] '(lambda() (interactive) (split-window-horizontally) (other-window 1)))
(global-set-key [(super a) ?t ?q ] 'util-toggle-quotes)
(global-set-key [(super a) ?u ?b] 'util-update-buffers)
(global-set-key [(super a) ?u ?t] 'untabify)
(global-set-key [(super a) ?w ?a] 'airmacs-agnostic-warn)
(global-set-key [(super e)] 'eval-region-verbose)
(global-set-key [(super shift e)] '(lambda()  (interactive)
                             (eval-region (region-beginning) (region-end)) (deactivate-mark)))
(global-set-key [(super f)] 'eval-fun)
(global-set-key [(super k)] 'util-kill-whole-line)
(global-set-key [(super tab)] 'yas-next-field)
(global-set-key [C-tab] 'yas-prev-field)
(global-set-key [C-backspace] 'util-backward-kill-word)
(global-set-key [C-down] '(lambda () (interactive) (next-line 5)))
(global-set-key [C-left] 'util-backward-word)
(global-set-key [C-next] 'util-jump-to-bottom)
(global-set-key [C-prior] 'util-jump-to-top)
(global-set-key [C-right] 'util-forward-word)
(global-set-key [C-s-down] '(lambda () (interactive) (copy-from-above-or-below 'nil 1)))
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
(global-set-key [M-up] '(lambda () (interactive) (util-goto-matching-char t)))
(global-set-key [down] 'next-line)
(global-set-key [end]      'util-goto-end)
(global-set-key [f10] '(lambda () (interactive) (comment-line 'comment-region)))
(global-set-key [f11] 'other-window)
(global-set-key [f12] 'font-lock-mode)
(global-set-key [f3] '(lambda () (interactive) (set-mark-command t)))
(global-set-key [f5] 'run-current-file)
(global-set-key [f6] 'next-error)
(global-set-key [f7] 'mdi-maximize-restore-toggle)
(global-set-key [f8] 'util-kill-this-buffer)
(global-set-key [home]     'util-goto-beg)
(global-set-key [insert] 'nil)
(global-set-key [kp-end]   'util-goto-end)
(global-set-key [kp-home]  'util-goto-beg)
(global-set-key [left] 'backward-char)
(global-set-key [right] 'forward-char)
(global-set-key [s-down] '(lambda () (interactive) (copy-from-above-or-below 1 1)))
(global-set-key [s-up] '(lambda () (interactive) (copy-from-above-or-below 1)))
(global-set-key [up] 'previous-line)

(global-set-key [s-kp-4] 'windmove-left) 
(global-set-key [s-kp-6] 'windmove-right) 
(global-set-key [s-kp-8] 'windmove-up) 
(global-set-key [s-kp-2] 'windmove-down)

(defun common-hook ()
  (local-set-key [tab] 'util-indent-region-or-line)
  (local-set-key [(return)] 'newline-and-indent)
  (yas-minor-mode))

;; Lisp specific stuff
(add-hook 'emacs-lisp-mode-hook 'common-hook)
(add-hook 'lisp-mode-hook 'common-hook)

;; javascript mode
(autoload 'js2-mode "js2" nil t)
(setq js2-use-font-lock-faces t)
(setq js2-mirror-mode nil)
(setq-default js2-basic-offset 2)
(setq-default js2-highlight-level 3)
(setq-default indent-tabs-mode nil)
(make-variable-buffer-local 'tab-width)
(setq-default js2-show-parse-errors nil)
(setq-default js2-strict-missing-semi-warning nil)
(setq-default js2-strict-trailing-comma-warning t)
(setq-default js2-global-externs '("setTimeout" "clearTimeout" "setInterval" "clearInterval" "console" "JSON" "define" "describe" "beforeEach" "afterEach" "it" "xit" "spyOn" "expect" "jasmine" "runs" "waits" "waitsFor" "xdescribe" "require" "localStorage" "sessionStorage" "Image" "exports" "module"))

(add-hook 'js2-mode-hook
          '(lambda ()
             (tern-mode t)
             (js2-imenu-extras-mode)
             (modify-syntax-entry ?\_ "w")
             (define-key
               js2-mode-map [tab] 'util-indent-region-or-line)
             (local-set-key [(return)] 'newline-and-indent)
             (local-set-key [(super a) ?e ?f] 'js2r-extract-function)
             (local-set-key [(super a) ?e ?v] 'js2r-extract-var)
             (local-set-key [(super a) ?e ?m] 'js2r-extract-method)
             (local-set-key [(super a) ?i ?p] 'js2r-introduce-parameter)
             (local-set-key [(super a) ?l ?p] 'js2r-localize-parameter)
             (local-set-key [(super a) ?c ?o] 'js2r-contract-object)
             (local-set-key [(super a) ?x ?a] 'js2r-expand-array)
             (local-set-key [(super a) ?x ?f] 'js2r-expand-function)
             (local-set-key [(super a) ?c ?f] 'js2r-contract-function)
             (local-set-key [(super a) ?c ?o] 'js2r-contract-object)
             (local-set-key [(super a) ?i ?v] 'js2r-inline-var)
             (local-set-key [(super a) ?r ?v] 'js2r-rename-var)
             (local-set-key [(super a) ?v ?t] 'js2r-var-to-this)
             (local-set-key [(super a) ?a ?o] 'js2r-arguments-to-object)
             (local-set-key [(super a) ?3 ?i] 'js2r-ternary-to-if)
             (local-set-key [(super a) ?s ?v] 'js2r-split-var-declaration)
             (local-set-key [(super a) ?s ?s] 'js2r-split-string)
             (local-set-key [(super a) ?u ?w] 'js2r-unwrap)
             (local-set-key [(super a) ?l ?t] 'js2r-log-this)
             (local-set-key [(super a) ?s ?l] 'js2r-forward-slurp)
             (local-set-key [(super a) ?b ?a] 'js2r-forward-barf)
             (local-set-key [(super a) ?k] 'js2r-kill)
             ))

(add-hook 'java-mode-hook
          '(lambda ()
            (progn
              (message "java-mode")
              (setq c-basic-offset 2
                    tab-width 2
                    indent-tabs-mode t))))

;; css mode
(add-hook 'css-mode-hook 'common-hook)
;; html/web mode
(add-hook 'html-mode-hook 'common-hook)
(add-hook 'web-mode-hook 'common-hook)

(defun in-js2-file () (derived-mode-p 'js2-mode))

(defun insert-if-js2 (str) 
  (if (in-js2-file)
      (progn
        (backward-delete-char 3)
        (insert str)
        (loop for i in str do (forward-char 1))
        "")))

(add-hook 'python-mode-hook 'common-hook)

(setenv "NODE_PATH" "/usr/local/lib/node_modules/")

;; make sure compilation buffers show colorz!
(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (toggle-read-only)
  (ansi-color-apply-on-region (point-min) (point-max))
  (toggle-read-only))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

(add-hook 'term-mode-hook (lambda()
                (yas-minor-mode -1)))
(add-hook 'prog-mode-hook 'yas-minor-mode)
(add-hook 'ess-mode-hook 'yas-minor-mode)
(add-hook 'markdown-mode-hook 'yas-minor-mode)  

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(web-mode-attr-indent-offset 2)
 '(web-mode-code-indent-offset 2)
 '(web-mode-css-indent-offset 2)
 '(web-mode-markup-indent-offset 2)
 '(web-mode-sql-indent-offset 2))

(add-hook 'conf-javaprop-mode-hook 
          '(lambda () (conf-quote-normal nil)))

(message "Done loading airmacs")
