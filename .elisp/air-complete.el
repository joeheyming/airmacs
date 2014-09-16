(provide 'air-complete)

(require 'cl)
;minibuffer auto-complete
(defun air-lookup-filename-completion ()
  (let ((completion
         (cadr
          (find-if (lambda (el) (looking-at (car el))) 
                   air-dir-completions))))
    (eval completion)))

(defun air-findfile-completion ()
  ;; Extension to the complete word facility of the minibuffer
  (interactive)
  (backward-char 3)
  (let ((directory (air-lookup-filename-completion)))
    (cond 
     (directory 
      (beginning-of-line)
      (delete-region (line-beginning-position) (line-end-position))
      (insert directory))
     (t (forward-char 3) (minibuffer-complete)))))

(message "loaded aircomplete")
