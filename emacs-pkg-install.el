;;
;; Install package from command line. Example:
;;
;;   $ emacs --batch --expr "(define pkgs-to-install 'smex)" -l emacs-pkg-install.el
;;
(require 'cl)
(require 'package)

(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)

;; Fix HTTP1/1.1 problems
(setq url-http-attempt-keepalives nil)

(package-refresh-contents)
(package-initialize)
(message "Installing packages:")
(loop for package in pkgs-to-install
      do
      (progn
        (message (format "\t%s" package)))
        (package-install package))
     
