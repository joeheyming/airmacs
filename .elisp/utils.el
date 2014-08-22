(provide 'utils)

;;;;;;;;;;;;;;;;;;
; fancy delete-other-windows
; from: http://www.cs.berkeley.edu/~smcpeak/elisp/scott.emacs.el
(defvar my-saved-window-config-list nil)
(defun mdi-maximize-restore-toggle ()
  "When called in a multi-window frame it will save the window
  configuration by calling `current-window-configuration', then call
  `delete-other-windows'.  When called in a single-window frame it will
  restore the frame configuration by calling `set-window-configuration'."
  (interactive)
  (if (> (count-windows) 1)
      (progn
        (gc-util-window-config-list (selected-frame))
        (setq util-saved-window-config-list
              (cons (list (buffer-name) (current-window-configuration))
                    util-saved-window-config-list))
        (delete-other-windows))
    (restore-applicable-window-configuration util-saved-window-config-list)))

(defun gc-util-window-config-list (frame)
  "Remove any saved configs that apply to deleted frames or to
  the 'frame' argument."
  (setq util-saved-window-config-list
    (filter-list util-saved-window-config-list
      #'(lambda (config)
          (and
            (member (window-configuration-frame (car (cdr config))) (frame-list))
            (not (eq (window-configuration-frame (car (cdr config))) frame))
          ))
    )))

(defun restore-applicable-window-configuration (list)
  "Look through 'list' for a window config that applies to the selected
  frame.  If found, restore via that config.  If not, say so."
  (if (not list)
      (princ "There is no saved window config for this buffer.")
    (let ((bufname (car (car list)))
          (windowconfig (car (cdr (car list)))))
      (if (and (eq (window-configuration-frame windowconfig) (selected-frame))
               (eq bufname (buffer-name)))
          ; restore it
          (set-window-configuration windowconfig)
        ; else, proceed down list
        (restore-applicable-window-configuration (cdr list))))))

(defun util-iswitchb-otherwindow ()
  (interactive)
  "Toggle new buffer in otherwindow setting"
  (let ((buffer  (car iswitchb-matches)))
    (if (not (eq iswitchb-method 'otherwindow))
        (progn
          (message "Other window: %s" buffer)
          (setq iswitchb-method 'otherwindow))
      (progn (message "Same window:  %s" buffer)
             (setq iswitchb-method 'always-frame)))))

(defun filter-list (list predicate)
  "Return a list containing only those elements from 'list' which
  cause 'predicate' to return true."
  (if (not list)
      nil          ; recursion base case
      (if (funcall predicate (car list))
          ; keep the item
          (cons (car list) (filter-list (cdr list) predicate))
          ; else, remove it
          (filter-list (cdr list) predicate)
      )))

(global-set-key [f7] 'mdi-maximize-restore-toggle)


(defun util-kill-this-buffer ()
  (interactive)
  (if (window-minibuffer-p) (keyboard-escape-quit)
    (if (string= "*Buffer List*" (buffer-name)) (keyboard-quit)
      (progn
        (kill-buffer (current-buffer))
        (if (> (count-windows) 1) (delete-window))
        ))))

(global-set-key [f8] 'util-kill-this-buffer)

(defun util-shell-function (cmd &optional buffername quiet)
  "Run a function defined in our bash configuration"
  (interactive (list (read-string "% ")))
  (if (not buffername) (setq buffername "*shell function output*"))
  (setq oldbuffer (buffer-name))
  (if (string= buffername "discard output") (setq buffername 'nil))
  (if (string= buffername "stdout")
      (setq buffername 't)
    (if buffername (util-select-empty-output-buffer buffername)))
  (when (not quiet) (message "Running: %s" cmd))
  (delete-other-windows)
  (call-process "sh" nil buffername nil
                 "-c"
                (format "cd ~; %s" cmd))
  (split-window-vertically)
  (other-window 1)
  (switch-to-buffer oldbuffer)
  (when (not quiet) (message "Done."))
  )
(defun util-shell-function-basic (cmd &optional args)
  (let ((bufname (format "*%s*" cmd)))
    (when args (setq cmd (format "%s %s" cmd args)))
    (util-shell-function cmd bufname)))
(defun util-shell-function-no-output (cmd &optional args)
  (when args (setq cmd (format "%s %s" cmd args)))
  (util-shell-function cmd "discard output" 't))
(defun util-shell-function-eval (cmd)
  "Evaluate a function and return its output"
  (with-output-to-string
    (with-current-buffer
        standard-output
      (util-shell-function cmd "stdout" 't))))

(defun util-select-empty-output-buffer (buffername)
  (switch-to-buffer (get-buffer-create buffername))
  (util-erase-buffer))

(defun util-erase-buffer ()
  (setq buffer-read-only 'nil)
  (erase-buffer))

(defun util-generate-empty-output-buffer (buffername)
  (let (current-buffer (current-buffer))
    (util-select-empty-output-buffer buffername)
    (switch-to-buffer current-buffer)))


(defun first-line-matching (regexp)
  (let ((line-char 'nil))
    (save-excursion
      (goto-char 0)
      (if (search-forward-regexp regexp 'nil 't)
          (setq line-char (line-beginning-position))
        )
      line-char
      )
    ))

(defun firstword ()
  (save-excursion
    (beginning-of-buffer)
    ( let ((beg) (end) )
      (setq beg (point))
      (search-forward-regexp "[ \n]")
      (setq end (- (point) 1))
      (buffer-substring beg end)
      )))
      
(defun shebang ()
  "return the shebang of the current buffer"
  (interactive)
  (save-excursion
    (let ((word (firstword)))
      (if (string-match "^#!.*" word)
          (progn
            (beginning-of-buffer)
            (search-forward-regexp "#!")
            (let ((beg) (end) )
              (setq beg (point))
              (search-forward-regexp "[ \n]")
              (setq end (- (point) 1))
              (buffer-substring beg end)
              )
            ))
      )
    ))

(defun util-save-and-save-some-buffers ()
  (if (buffer-file-name) (save-buffer))
  (save-some-buffers))

(defun run-current-file () 
  (interactive)
  (let ((bang (shebang)))
    (if (not(string= bang ""))
        (progn
          (util-save-and-save-some-buffers)
          (util-shell-function 
           (format "%s %s" bang buffer-file-name)
           )
          )
      )
    )
)


(global-set-key [f5] 'run-current-file)

(defun util-zap-to-char (arg char)
  "Kill up to *but not including* ARG'th occurrence of CHAR.
Case is ignored if `case-fold-search' is non-nil in the current buffer.
Goes backward if ARG is negative; error if CHAR not found."
  (interactive "p\ncZap to char: ")
  (kill-region (point) (progn
             (search-forward (char-to-string char) nil nil arg)
                         (backward-char 1)
             (point))))

; don't iconify on C-z when running in X
(when window-system (global-set-key "\C-z" 'util-zap-to-char))

(defun isearch-occur ()
  "Invoke `occur' from within isearch."
  (interactive)
  (let ((case-fold-search isearch-case-fold-search))
    (occur (if isearch-regexp isearch-string (regexp-quote isearch-string)))))


;;(defun util-toggle-quotes ()
;;  "Converts double quotes to singles and vice versa"
;;  (interactive)
;; (save-excursion
;;    (let ((start (point))
;;          (face_at_point (last-element (face-at-point)))
;;         beg end)
;;      (while (eq face_at_point (last-element (face-at-point)))
;;        (forward-char -1))
;;      (forward-char 1)
;;      (while (looking-at "[ \t\n]") (forward-char 1))
;;      (setq beg (point))
;;      (if (not (looking-at "[\"\']")) (search-forward-regexp "[\"\']" 'nil 't))
;;      (if (<= (point) start) (setq beg (point)))
;;
;;      (goto-char start)
;;
;;      (while (eq face_at_point (last-element (face-at-point)))
;;        (forward-char 1))
;;      (forward-char -1)
;;     (while (looking-at "[ \t\n]") (forward-char -1))
;;      (setq end (point))
;;      (if (not (looking-at "[\"\']")) (search-backward-regexp "[\"\']" 'nil 't))
;;      (if (>= (point) start) (setq end (point)))
;;
;;      (goto-char beg)
;;      (if (looking-at "\"")
;;          (progn (delete-char 1) (insert-char ?\047 1))
;;        (if (looking-at "\'")
;;            (progn (delete-char 1) (insert-char ?\042 1))
;;          (insert-char ?\042 1)))
;;
;;      (goto-char end)
;;      (if (looking-at "\"")
;;          (progn (delete-char 1) (insert-char ?\047 1))
;;        (if (looking-at "\'")
;;            (progn (delete-char 1) (insert-char ?\042 1))
;;          (progn (forward-char 2) (insert-char ?\042 1))))

(defun current-line-full ()
  (buffer-substring (line-beginning-position) (+ 1 (line-end-position))))

(defun util-comment-dwim ()
  (interactive)
  (if (not mark-active)
      (progn
        (if (eq last-command 'util-comment-dwim)
            (kill-append (current-line-full) 'nil)
          (kill-new (current-line-full)))
        (comment-region (line-beginning-position) (line-end-position))
        (forward-line 1)
        )
    (comment-dwim nil)))

(defun util-kill-this-buffer ()
  (interactive)
  (if (window-minibuffer-p) (keyboard-escape-quit)
    (if (string= "*Buffer List*" (buffer-name)) (keyboard-quit)
      (progn
        (kill-buffer (current-buffer))
        (if (> (count-windows) 1) (delete-window))
        ))))

(defun util-kill-line-or-region ()
  "Kill region if active, otherwise kill line"
  (interactive)
  (if mark-active (kill-region (mark) (point)) (kill-line)))

(defun util-kill-word ()
  "Kill characters forward until the end of a word or line"
  (interactive)
  (let (endofword (endofline (line-end-position)))
    (save-excursion (forward-word 1) (setq endofword (point)))
    (kill-region (point) 
                 (if (= endofline (point))
                     (1+ endofline)
                   (if (< endofline endofword) endofline endofword)))))

(defun util-backward-kill-word ()
  "Kill characters backward until the beg of a word or line"
  (interactive)
  (let (begofword (begofline (line-beginning-position)))
    (save-excursion (forward-word -1) (setq begofword (point)))
    (kill-region (point) 
                 (if (= begofline (point))
                     (1- begofline)
                   (if (> begofline begofword) begofline begofword)))))

(defun util-forward-word ()
   ;; Move one word forward. Leave the pointer at start of word
   ;; instead of emacs default end of word. Treat _ as part of word
   (interactive)
   (let ((start (point)) boundary at-boundary jump)
     (setq at-boundary (eolp))
     (setq boundary (line-end-position))
     (forward-char 1)
     (backward-word 1)
     (forward-word 2)
     (backward-word 1)
     (backward-char 1)
     (cond ((or (looking-at "_") (looking-at "\\.[0-9]\\.")) (forward-char 1) 
            (util-forward-word))
           (t (forward-char 1)))
     (if (and (not at-boundary) (> (point) boundary)) (goto-char boundary))
     (setq jump (util-count-lines start (point)))
     (if (> jump 0) (progn (forward-line (- 1 jump)) (back-to-indentation)))
     ))
(defun util-backward-word ()
   ;; Move one word backward. Leave the pointer at start of word
   ;; Treat _ as part of word
   (interactive)
   (let ((start (point)) boundary at-boundary jump)
     (setq at-boundary (empty-line-prefix))
     (setq boundary (line-beginning-position))
     (backward-word 1)
     (backward-char 1)
     (cond ((or (looking-at "_") (looking-at "\\.[0-9]\\.")) 
            (util-backward-word))
           (t (forward-char 1)))
     (if (and (not at-boundary) (< (point) boundary)) 
         (progn (goto-char boundary) (back-to-indentation)))
     (setq jump (util-count-lines start (point)))
     (if (> jump 0) (progn (forward-line (- jump 1)) (end-of-line)))
     ))

(defun util-count-lines (beg end)
  (let (tmp)
    (if (< end beg) (progn (setq tmp beg) (setq beg end) (setq end tmp)))
    (save-excursion 
      (goto-char beg) (setq beg (line-beginning-position))
      (goto-char end) (setq end (line-beginning-position))
      )
    (count-lines beg end)))

(defun current-line ()
  (buffer-substring (line-beginning-position) (line-end-position)))
(defun current-line-full ()
  (buffer-substring (line-beginning-position) (+ 1 (line-end-position))))
(defun current-line-prefix ()
 (buffer-substring (line-beginning-position) (point)))
(defun current-line-suffix () (buffer-substring (point) (line-end-position)))
(defun current-line-number ()
  (let ((linenum (string-to-int (substring (what-line) 5))))
    (message "")
    linenum))
(defun current-number ()
  (save-excursion
    (let (beg)
      (skip-chars-backward "0-9")
      (setq beg (point))
      (skip-chars-forward "0-9")
      (buffer-substring beg (point)))))

(defun empty-line-suffix () (only-whitespace (current-line-suffix)))
(defun empty-line-prefix () (only-whitespace (current-line-prefix)))

(defun only-whitespace (str) (and (string-match "^[ \r\t]*\$" str) 't))
(defun next-char ()
  (if (>= (point) (1- (point-max)))
      (current-char)
    (char-to-string (char-after (1+ (point))))))
(defun current-char ()
  (char-to-string (following-char)))
(defun previous-char ()
  (char-to-string (preceding-char)))
(defun previous-string (&rest strlist)
  (let (found length)
    (loop for str in (flatten strlist) do
          (setq length (length str))
          (and (not found) (> length 0) (< length (point))
               (save-excursion
                 (backward-char length)
                 (when (looking-at str) (setq found str)))))
    found
    ))
(defun previous-key (&optional arg)
  (if (not arg) (setq arg 1))
  (let (recent-keys index)
    (setq recent-keys (recent-keys))
    (setq index (- (length recent-keys) (1+ arg)))
    (if (>= index 0)
        (aref recent-keys index)
      'nil)))

(defun util-chord-for-key (key)
  (key-description key))

(defun previous-key-string (&optional arg)
  (util-chord-for-key (vector (previous-key arg))))

(defun util-goto-end (&optional ARG)
  (interactive)
  (let ((prevkey (previous-key-string)))
    (if (or (string= prevkey "<end>")
            (string= prevkey "<kp-end>")
            (and (string= (previous-key-string 7) "ESC")
                 (string= (previous-key-string 6) "[")
                 (string= (previous-key-string 5) "4")
                 (string= (previous-key-string 4) "~")))
        (end-of-buffer ARG)
      (end-of-line ARG))))

(defun util-goto-beg (&optional ARG)
  (interactive)
  (let ((prevkey (previous-key-string)))
    (if (or (string= prevkey "<home>")
            (string= prevkey "<kp-home>")
            (and (string= (previous-key-string 7) "ESC")
                 (string= (previous-key-string 6) "[")
                 (string= (previous-key-string 5) "1")
                 (string= (previous-key-string 4) "~")))
        (beginning-of-buffer ARG)
      (beginning-of-line ARG))))

(defun copy-from-above-or-below (&optional arg below)
  "Copy characters from previous nonblank line, starting just above point.
Copy ARG characters, but not past the end of that line.
If no argument given, copy the entire rest of the line.
The characters copied are inserted in the buffer before point."
  (interactive "P")
  (let ((cc (current-column)) n (string "") wordlen)
    (save-excursion
      (if below (progn (end-of-line) (skip-chars-forward "\ \t\n"))
        (progn (beginning-of-line) (skip-chars-backward "\ \t\n")))
      (move-to-column cc)
      ;; Default is enough to copy the whole rest of the line.
      (save-excursion
        (let ((start (point)))
          (forward-word 1)
          (setq wordlen (- (point) start))))
      (setq n (if arg wordlen (point-max)))
      ;; If current column winds up in middle of a tab,
      ;; copy appropriate number of "virtual" space chars.
      (if (< cc (current-column))
	  (if (= (preceding-char) ?\t)
	      (progn
		(setq string (make-string (min n (- (current-column) cc)) ?\ ))
		(setq n (- n (min n (- (current-column) cc)))))
	    ;; In middle of ctl char => copy that whole char.
	    (backward-char 1)))
      (setq string (concat string
			   (buffer-substring
			    (point)
			    (min (save-excursion (end-of-line) (point))
				 (+ n (point)))))))
    (insert string)))

(defun util-toggle-kbd-macro-recording ()
  (interactive)
  (if defining-kbd-macro (end-kbd-macro) (start-kbd-macro 'nil)))
