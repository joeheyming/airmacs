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
(loop for package in pkgs-to-install
      do
      (condition-case err
          (progn
            (message (format "\t%s" package))
            (package-install package)
            )
        (error
         err
         (message "Error installing package: %s\n%s" package err)
         )
        )
      )
     

(require 'emojify)
(emojify-download-emoji emojify-emoji-set)
