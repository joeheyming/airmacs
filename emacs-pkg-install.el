;;
;; Install package from command line. Example:
;;
;;   $ emacs --batch --expr "(define pkgs-to-install 'smex)" -l emacs-pkg-install.el
;;
(require 'cl)

;; Fix HTTP1/1.1 problems
(setq url-http-attempt-keepalives nil)

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)
(package-refresh-contents)

(message "Installing packages:")
(dolist (pkg argv)
  (if (package-installed-p pkg)
      (message (format "\t%s is already installed" pkg))
    (condition-case nil
        (package-install (intern pkg) t)
      (error nil))))

(require 'emojify)
(unless (file-exists-p (concat emojify-emojis-dir "/" emojify-emoji-set))
  (emojify-download-emoji emojify-emoji-set))
