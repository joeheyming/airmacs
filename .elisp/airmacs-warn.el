;; A library for adding a warn/log statements for text under your cursor
;;    or selected text

(provide 'airmacs-warn)

(defun airmacs-pythonprint-variable (varname variable)
  (format "print '%s = %%s' %% (%s)" varname variable))

(defun airmacs-consolelog-variable (varname variable)
  (format "console.log('%s = ', %s);" varname variable))

(defun airmacs-javalog-variable (varname variable)
  (format "log.warn(\"%s = \" + %s);" varname variable))

(defun airmacs-phpprint-variable (varname variable)
  (format "print(\"%s = \" . %s);" varname variable))

(defun airmacs-perl-warn-data-dumper (varname variable)
  (cond
   ((or (string-match "@" variable) (string-match "%" variable))
    (format "warn Data::Dumper->Dump([\\%s], [qw(%s)]);" variable varname))
   (t (format "warn Data::Dumper->Dump([%s], [qw(%s)]);" variable varname))
   )
)

(defun airmacs-shecho-variable (varname variable)
  (format "echo \"%s = $%s\"" varname variable))

(defun airmacs-rubyprint-variable (varname variable)
  (format "print %s" variable))

(defun airmacs-lisp-format-variable (varname variable)
  (format "(message (format \"%s = %%s\" %s))" varname variable))

(defun airmacs-go-format-variable (varname variable)
  (format "log.Println(fmt.Sprintf(\"%s = %%s\", %s))" varname variable))


(defun airmacs-astrace-variable (varname variable)
  (format "trace(\"%s = \" + %s);" varname variable))

(defun airmacs-groovyprint-variable (varname variable)
  (format "println(\"%s = \" + %s);" varname variable))


(defun airmacs-insert-agnostic-warn (formatter)
  "Put in a pretty printed warn statement"
  (let (variable string go_up)
    (setq variable (replace-regexp-in-string  ":$" "" (if mark-active (active-region) (current-variable))))
    (setq string (current-word))
    (setq go_up nil)
    ;; If the current line has a return statement, insert the warn above this line.
    (save-excursion
      (beginning-of-line)
      (setq beg (point))
      (end-of-line)
      (setq current_line (buffer-substring beg (point)))
      (if (string-match "return" current_line)
          (setq go_up t)))
    (if go_up (backward-line))
    (end-of-line)
    (newline)
    (insert (funcall formatter string variable))
    (indent-according-to-mode)
    )
  )

(defun airmacs-agnostic-warn ()
  "Put a pretty printed statment for all languages"
  (interactive)
  (save-excursion
    (let ((mymode (format "%s" major-mode))
          (airmacs-formatter))
      (cond
       ((string-match "emacs-lisp" mymode)
        (setq airmacs-formatter 'airmacs-lisp-format-variable))
       ((string-match "go-mode" mymode)
        (setq airmacs-formatter 'airmacs-go-format-variable))
       ((string-match "python" mymode)
        (setq airmacs-formatter 'airmacs-pythonprint-variable))
       ((string-match "php" mymode)
        (setq airmacs-formatter 'airmacs-phpprint-variable))
       ((string-match "ruby" mymode)
        (setq airmacs-formatter 'airmacs-rubyprint-variable))
       ((string-match "perl" mymode)
        (setq airmacs-formatter 'airmacs-perl-warn-data-dumper))
       ((string-match "js" mymode)
        (setq airmacs-formatter 'airmacs-consolelog-variable))
       ((string-match "java" mymode)
        (setq airmacs-formatter 'airmacs-javalog-variable))
       ((string-match "sh-mode" mymode)
        (setq airmacs-formatter 'airmacs-shecho-variable))
       ((string-match "actionscript" mymode)
        (setq airmacs-formatter 'airmacs-astrace-variable))
       ((string-match "groovy" mymode)
        (setq airmacs-formatter 'airmacs-groovyprint-variable))
       )
      (airmacs-insert-agnostic-warn airmacs-formatter))))
