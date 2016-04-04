(provide 'air-utils)

;;;;;;;;;;;;;;;;;;
; fancy delete-other-windows
; from: http://www.cs.berkeley.edu/~smcpeak/elisp/scott.emacs.el
(defvar util-saved-window-config-list nil)
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

(defun util-kill-this-buffer ()
  (interactive)
  (if (window-minibuffer-p) (keyboard-escape-quit)
    (if (string= "*Buffer List*" (buffer-name)) (keyboard-quit)
      (progn
        (kill-buffer (current-buffer))
        (if (> (count-windows) (util-count-buffers)) (delete-window))
        ))))

;; Stolen wholesale http://www.cb1.com/~john/ (John Sturdy)
(defun util-count-buffers (&optional display-anyway)
  "Display or return the number of buffers."
  (interactive)
  (let ((buf-count (length (buffer-list))))
    (if (or (interactive-p) display-anyway)
    (message "%d buffers in this Emacs" buf-count)) buf-count))

;; Override this with a regex to add your own special buffers.
(setq util-special-buffers "")

(defun util-delete-other-buffers ()
  "Delete other buffers, but use a regex to prevent special buffers from being deleted."
  (interactive)
  (util-save-and-save-some-buffers)
  (let ((tobe-killed (cdr (buffer-list (current-buffer)))))
     (while tobe-killed
       (let ((current (car tobe-killed)))
         (if (not (string-match util-special-buffers (buffer-name current)))
             (progn
               (kill-buffer current)
               (message (format "kill buffer: %s" current))))
         (setq tobe-killed (cdr tobe-killed)))))
  (delete-other-windows))

(defun util-kill-other-buffers ()
      "Kill all other buffers."
      (interactive)
      (mapc 'kill-buffer (delq (current-buffer) (buffer-list))))

(defun util-shell-function (cmd &optional buffername quiet)
  "Run a function defined in our bash configuration"
  (interactive (list (read-string "% ")))
  (compile cmd)
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

(defun eval-fun()
  "Looks for the above defun, then evaluates the function"
  (interactive)
  (save-excursion
    (search-backward-regexp "^\(defun")
    (let ((line (current-line)))
      (string-match "defun \\([^(]+\\)" line)
      (message (format "evaling function: '%s'" (match-string 1 line))))
    
    (set-mark (point))
    (util-goto-matching-char)
    (forward-char)
    (eval-region (region-beginning) (region-end))
    (deactivate-mark)
    ))
  

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
  (interactive)
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

(defun util-zap-to-char (arg char)
  "Kill up to *but not including* ARG'th occurrence of CHAR.
Case is ignored if `case-fold-search' is non-nil in the current buffer.
Goes backward if ARG is negative; error if CHAR not found."
  (interactive "p\ncZap to char: ")
  (kill-region (point) (progn
             (search-forward (char-to-string char) nil nil arg)
                         (backward-char 1)
             (point))))

(defun isearch-occur ()
  "Invoke `occur' from within isearch."
  (interactive)
  (let ((case-fold-search isearch-case-fold-search))
    (occur (if isearch-regexp isearch-string (regexp-quote isearch-string)))))


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

(defun util-beginning-or-toindent ()
  (interactive)
  (if (and (eq (current-column) 0)
           (string= (previous-key-string) "C-a"))
      (back-to-indentation)
    (beginning-of-line)))

(defun end-of-line-ignore-whitespace ()
  (interactive)
  (goto-char (line-end-position-ignore-whitespace)))

(defun line-end-position-ignore-whitespace ()
  (save-excursion
    (end-of-line)
    (while (or (string= (previous-char) " ")
               (string= (previous-char) "	"))
      (backward-char 1))
    (point)))

(defun util-ending-or-nextline-end ()
  (interactive)
  (if (string= (previous-key-string) "C-e") (forward-line 1))
  (end-of-line-ignore-whitespace))


(defun util-jump-to-top ()
  (interactive)
  (goto-line (- (current-line-number) (window-line))))
(defun util-jump-to-bottom ()
  (interactive)
  (goto-line (- (+ (current-line-number) (window-line-from-bottom)) 1)))

(defun util-scootch-up ()
  (interactive)
  (util-scootch -1))
(defun util-scootch-down ()
  (interactive)
  (util-scootch 1))
(defun util-scootch-left ()
  (interactive)
  (util-scootch -1 't))
(defun util-scootch-right ()
  (interactive)
  (util-scootch 1 't))
(defun util-scootch (linecount &optional horizontal)
  (let ((col (current-column)) mark-was-active beg end region)
    (if mark-active
        (progn
          (setq mark-was-active t)
          (if (< (point) (mark)) (exchange-point-and-mark))
          (setq beg (mark))
          (setq end (point))
          )
      (progn
        (setq beg (line-beginning-position))
        (setq end (1+ (line-end-position)))
        ))
    (setq region (buffer-substring beg end))
    (delete-region beg end)
    (if horizontal (forward-char linecount) (forward-line linecount))
    (setq beg (point))
    (insert region)
    (if mark-was-active 
        (progn
          (goto-char beg)
          (setq deactivate-mark 'nil)
          (set-mark (point))
          (goto-char (+ (point) (length region)))
          )
      (progn 
        (if horizontal (forward-char linecount) (forward-line linecount))
        (move-to-column col)
        ))))

(defun util-matching-char-position (matchchar closechar backward)
  (save-excursion
    (let ((count 0) currchar pos
          (regexp (concat (regexp-quote matchchar) "\\|" (regexp-quote closechar)))
          (myface (face-at-point))
          )
      (while (not pos)
        (if backward
            (search-backward-regexp regexp)
          (progn
            (forward-char 1)
            (search-forward-regexp regexp)
            (backward-char 1)
            ))
        (if (not (equal (face-at-point) myface)) 'nil
          (if (string= (current-char) matchchar)
              (setq count (1- count))
            (if (= count 0) (setq pos (point))
              (setq count (1+ count))))
          (if (string= matchchar closechar)
              (setq pos (point))
            )
          ))
      pos
      )))

(defun util-find-matching-position (&optional opt-backward)
  (save-excursion
    (let ((matchchar (current-char)) closechar (backward opt-backward) pos)
      (when (string= matchchar "(") (setq closechar ")"))
      (when (string= matchchar ")") (setq closechar "(") (setq backward 't))
      (when (string= matchchar "[") (setq closechar "]"))
      (when (string= matchchar "]") (setq closechar "[") (setq backward 't))
      (when (string= matchchar "{") (setq closechar "}"))
      (when (string= matchchar "}") (setq closechar "{") (setq backward 't))
      (when (string= matchchar "<") (setq closechar ">"))
      (when (string= matchchar ">") (setq closechar "<") (setq backward 't))
      (when (string= matchchar "/") (setq closechar "/"))
      (when (string= matchchar "\"") (setq closechar "\""))
      (when (string= matchchar "\'") (setq closechar "\'"))
      (if (not closechar) (error "Not on a blinkable char, try one of '(){}[]<>/\'\"'"))
      (condition-case nil
          (util-matching-char-position matchchar closechar backward)
        (error (error "Couldn't find matching '%s'." closechar))))))

(defun util-goto-matching-char (&optional opt-backward)
  (interactive)
  (let ((pos (util-find-matching-position opt-backward)))
    (goto-char pos)))

(defun util-blink-matching-char ()
  (interactive)
  (save-excursion
    (util-goto-matching-char)
    (if blink-matching-paren-on-screen
        (if (pos-visible-in-window-p) (sit-for 0.5)
          (message "Matches: %s" (current-line))))))


(defun util-apply-hunk (&optional revert)
  (interactive)
  (save-excursion
    (let ((filename (diff-find-file-name)) errormsg)
      (condition-case err (diff-apply-hunk revert)
        (error (setq errormsg (error-message-string err))))
      (save-other-buffer (get-file-buffer filename))
      (when (and errormsg (not (string= errormsg "No next hunk")))
        (message errormsg)))))

(defun util-revert-hunk ()
  (interactive)
  (util-apply-hunk 't))

(defun util-revert-file ()
  (interactive)
  (save-excursion
    (beginning-of-buffer)
    (search-forward-regexp "[iI]ndex")
    (util-walk-diff-hunks 'util-revert-hunk)))
(defun util-apply-file ()
  (interactive)
  (save-excursion
    (util-walk-diff-hunks 'util-apply-hunk)))
(defun util-walk-diff-hunks (func)
  (let (beg)
    (beginning-of-line 1)
    (if (looking-at "[Ii]ndex:? ") (forward-line))
    (search-backward-regexp "[Ii]ndex:? ")
    (forward-line)
    (setq beg (point))
    (condition-case nil (search-forward-regexp "^[Ii]ndex:? ")
      (error (goto-char (point-max))))
    (while (> (point) beg)
      (progn
        (condition-case nil (search-backward-regexp "^@@ ")
          (error (goto-char (point-min))))
        (if (> (point) beg) (funcall func))))))

(defun active-region ()
  (buffer-substring (point) (mark)))
(defun current-keyword-or-quoted-active-region (&optional f)
  (if mark-active (concat "'" (active-region) "'")
    (let ((string (or (current-word nil t) "")))
      (if f (funcall f string) string))))

(defun string-join (str_list &optional join_str)
  "Joins a list of strings"
  (mapconcat 'identity str_list (or join_str " "))
  )

(defun save-other-buffer (buffer)
  (with-current-buffer buffer
    (progn
      (set-buffer-modified-p 't)
      (save-buffer))))

(defun chomp (str)
      "Chomp leading and tailing whitespace from STR."
      (while (string-match "\\`\n+\\|^\\s-+\\|\\s-+$\\|\n+\\'"
                           str)
        (setq str (replace-match "" t t str)))
      str)

(defun util-update-buffers ()
  "Refreshs all open buffers from their respective files"
  (interactive)
  (let* ((list (buffer-list)) (buffer (car list)) errmesg)
    (loop for buffer in (buffer-list) do
          (if (and (not (string-match "\\*" (buffer-name buffer)))
                   (buffer-file-name buffer)
                   (file-exists-p (buffer-file-name buffer)))
              (if (and (not (verify-visited-file-modtime buffer)) ; been touched
                       (buffer-modified-p buffer)) ; and modified 
                  (setq errmesg (concat errmesg 
                    (format "Buffer '%s' has file and buffer changes!\n" buffer)))
                (util-update-buffer buffer))))
    (if errmesg (setq errmesg (chomp errmesg)))
    (message "%s" (or errmesg "Done refreshing all open non-modified files..."))))

(defun util-update-buffer (buffer)
  (set-buffer buffer) 
  (message "Refreshing %s" (buffer-file-name buffer))
  (if (not (verify-visited-file-modtime buffer))
      (if (buffer-modified-p buffer) (error "Buffer has file and buffer changes")
        (revert-buffer t t t))) ; revert if touched and not modified
  (vc-file-clearprops (buffer-file-name)))


(defun fix_quotes (tofix)
  (let (variable beg_c end_c variable_len)
    (setq variable tofix)
    (setq variable_len (length variable))
    (setq beg_c (substring tofix 0 1))
    (setq end_c (substring tofix (- variable_len 1)))
    ;; handle the case where the beginning is quote, but the end is not
    (if (and 
         (not (equal end_c "'"))
         (equal beg_c "'"))
        (setq variable (format "%s'" variable)))
    ;; handle the case where the end is quote, but the beginning is not
    (if (and 
         (not (equal beg_c "'"))
         (equal end_c "'"))
        (setq variable (format "'%s" variable)))
    variable)
  )

;; current-variable matches this:
;; perl: $var, @var, %var, $C'VAR, 'asdf', $var[0]
;;  with expressions like @{$var{foo}}, depending on your cursor, you can get $var or foo
;;   $other->[0] returns only $other, and $other->($a, $b) returns $other
;;   $other->()[0] only returns $other
;; javascript: somevar, namespace.foo.var, bar[0]
;;   foo(a, b) only gets foo, a, or b depending on your cursor
;; 'some str' will match 'some' when over some, 'str' when over str
;; pretty much, anything but complex function calls
(defun current-variable ()
  (save-excursion
    (skip-chars-backward "\':@$%[A-Za-z0-9_\.")
    (let (beg end variable)
      (setq beg (point))
      (skip-chars-forward "\':$[]@%A-Za-z0-9_\.")
      (setq end (point))
      (setq variable (fix_quotes (buffer-substring beg end)))
      )
    ))

;; current-expression matches this:
;; perl: $var, @var, %var, $C'VAR, @{$var{foo}}, 'asdf', $bar[{$var{foo}}] +{}, +()
;;   $other->[0] returns $other->[0], and $other->($a, $b) returns $other->($a, $b)
;;   $other->()[0] only return s $other->()
;; javascript: somevar, namespace.foo.var, bar[0]
;;   foo(a, b) only works up to the last paren, so
;;   foo(a, b) will match foo(a, b), but foo(a, b)[0] will only match foo(a, b)
;; 'some str' will match 'some' when over some, 'str' when over str
;; pretty much, anything but complex function calls
(defun current-expression ()
  (save-excursion
    (skip-chars-backward "\':{[]}@$%A-Za-z0-9_\.\\->")
    (let (beg end variable variable_len end_c)
      (setq beg (point))
      (skip-chars-forward "\'{}:$@%A-Za-z0-9_\.\\->")
      (setq end (point))
      (setq variable (buffer-substring beg end))
      (setq variable_len (length variable))
      ;; handle function calls
      (setq end_c (buffer-substring end (+ end 1)))
      (if (or (equal end_c "(") (equal end_c "[") (equal end_c "{") )
          (progn
            (setq beg (point))
            (forward-list) ;; go to the ending paren
            (setq end (point))
            (setq variable (format "%s%s" variable (buffer-substring beg end)))
            )
          )
      ;; make sure -> doesn't trail
      (if (equal (substring variable (- variable_len 2)) "->")
          (setq variable (substring variable 0 (- variable_len 2))))
      (fix_quotes variable))))

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
      (buffer-substring-no-properties beg (point)))))

(defun util-mark-whole-line ()
  (interactive)
  (push-mark (line-beginning-position))
  (forward-line 1)
  (beginning-of-line)
  (exchange-point-and-mark)
  (exchange-point-and-mark))

(defun util-kill-whole-line ()
  (interactive)
  (util-mark-whole-line)
  (kill-region (mark) (point)))

(defun util-pretty-xml ()
  "Prettify ugly xml in buffer"
  (interactive)
  (save-excursion
    (beginning-of-buffer)
    (while (search-forward-regexp "><" 'nil t)
      (backward-char)
      (insert "\n"))
    (mark-whole-buffer)
    (indent-for-tab-command)
    (while (search-forward-regexp "\" " 'nil t)
      (if (> (current-column) 80)
          (progn
            (insert "\n")
            (indent-for-tab-command)
            )))))

(defun util-ensure-trailing-thing (thing)
  "Toggle a trailing thing on the line"
  (interactive)
  (save-excursion
    (end-of-line)
    (unless (bobp)
      (backward-char 1)
      (cond
       ((looking-at thing)
        (delete-char 1)
        t)
       (t
        (forward-char 1)
        (insert thing)))))
  (next-line 1))

(defun util-generic-indent-region-or-line (indent-command)
  (if mark-active
      (progn  ; set beg end to marked region
        (if (< (point) (mark)) (exchange-point-and-mark))
        (indent-region (mark) (1- (point)))
        )
    (let ((previous-key (previous-key)))
      (if (and (or (looking-at "\\>")
                   (empty-line-suffix)
                   (string-equal (previous-char) ":")
                   (eq previous-key 'left)
                   (eq previous-key 'right))
               (not (eq previous-key 'down))
               (not (eq previous-key 'up))
               (not (and (eolp) (string-match "[\]\)\}\,]" (previous-char))))
               (not (empty-line-prefix)))
          (hippie-expand 'nil)
        (progn
          (setq he-num -1)
         (funcall indent-command))))))


(defun util-indent-region-or-line ()
  (interactive)
  (util-generic-indent-region-or-line (function indent-according-to-mode)))

(defun util-try-expand-hashitems (old)
  (let (wordsize)
    (if (not old)
        (progn
          (if (gethash (buffer-substring (- (point) 4) (point)) util-tab-completions)
              (setq wordsize 4)
            (if (gethash (buffer-substring (- (point) 3) (point)) util-tab-completions)
                (setq wordsize 3)
              (if (gethash (buffer-substring (- (point) 2) (point)) util-tab-completions)
                  (setq wordsize 2)
                (setq wordsize 1))))
          (he-init-string (- (point) wordsize) (point))
          (if (not (he-string-member he-search-string he-tried-table))
              (setq he-tried-table (cons he-search-string he-tried-table)))
          (let ((item (gethash he-search-string util-tab-completions)))
            (setq he-expand-list
                  (cond
                   ((functionp item) (list (funcall item)))
                   (t item))))))
    (while (and he-expand-list
                (he-string-member (car he-expand-list) he-tried-table))
      (setq he-expand-list (cdr he-expand-list)))
    (if (null he-expand-list)
        (progn (if old (he-reset-string)) 'nil)
      (progn
        (he-substitute-string (car he-expand-list))
        (setq he-expand-list (cdr he-expand-list))
        t))))


(defvar util-tab-completions (make-hash-table :test 'equal))
(defun util-populate-hash (hash pairs)
  (loop for keyvalue in pairs do
        (let ((key (car keyvalue)) (value (cadr keyvalue)))
          (puthash key value hash))))

(defun util-region-replace (&optional from to)
  (interactive)
  (util-region-replace-func 'search-forward 'nil from to))
(defun util-region-replace-regex (&optional from to)
  (interactive)
  (util-region-replace-func 'search-forward-regexp t from to))
(defun util-region-replace-func (func asregex &optional from to)
  (let ((beg (point-min)) (end (point-max)) (region "Buffer") begend lastm done)
    (when mark-active
      (setq region "Region")
      (setq beg (point))
      (setq end (mark))
      (when (> beg end)
        (setq begend beg)
        (setq beg end)
        (setq end begend)))
    ;; convert 'end' to a marker object so that it gets adjusted properly across text replacement
    (setq end (copy-marker end))
    (if asregex (setq region (format "%s regexp" region)))
    (if (not from)
        (setq from (read-string (format "%s replace: " region))))
    (if (not to)
        (setq to (read-string (format "%s replace %s with: " region from))))
    (goto-char beg)
    (while (and (< (point) (marker-position end)) (not done))
      (setq lastm (point))
      (if (not (funcall func from nil t))
          (setq done 1))
      (if (<= (point) (marker-position end)) (replace-match to nil nil)))
    (goto-char lastm)
    ))

(defun util-vc-root ()
  (interactive)
  (condition-case nil
      (let ((backend 'Git))
	(message "Backend %s" backend)
        (vc-call-backend backend 'root (file-name-directory (buffer-file-name))))
    (error nil)))

(defun vc-root-or-current-dir ()
  "Get the vc root or the current directory"
  (interactive)
  (let ((the-root (util-vc-root)))
    (message (format "root: %s" the-root))
    (if the-root
      the-root
      (file-name-directory (or load-file-name buffer-file-name)))))

(defun strip-c-apostrophe (s) (replace-regexp-in-string "^C'" "" s))

(defun findcode-on (my-findcode-command) 
  "Delegate findcode to"
  (message (format "Findcode: %s" my-findcode-command))
  (let ((compilation-buffer-name-function
         (lambda (mode-name)
           (format "*%s*" my-findcode-command)))
        (default-directory (vc-root-or-current-dir)))
    (message (format "Default directory: %s" default-directory))
    (grep my-findcode-command)))

(setq findcode-command "grep --mmap -Rin %s .")
(defun util-findcode (findcode-command)
  "Run findcode on your current repo, or default path, if defined"
  (interactive
   (list (read-string
          (concat "Run findcode in " (vc-root-or-current-dir) " as: ")
          (format findcode-command (current-keyword-or-quoted-active-region 'strip-c-apostrophe)))))
  (findcode-on findcode-command))

(defun findgrep-on (my-findgrep-command) 
  "Delegate findgrep to"
  (message (format "Findgrep: %s" my-findgrep-command))
  (let ((compilation-buffer-name-function
         (lambda (mode-name)
           (format "*%s*" my-findgrep-command)))
        (default-directory (vc-root-or-current-dir)))
    (message (format "Default directory: %s" default-directory))
    (compile my-findgrep-command)))

(setq findgrep-command "find %s | grep -i %s")
(defun util-findgrep (findgrep-command)
  "Run findgrep on your current repo, or default path, if defined"
  (interactive
   (list (read-string
          (concat "Run findgrep in " (vc-root-or-current-dir) " as: ")
          (format findgrep-command (vc-root-or-current-dir) (current-keyword-or-quoted-active-region 'strip-c-apostrophe)))))
  (findgrep-on findgrep-command))


(defun chmod (mode)
  "Set the mode of the current file, in octal, as per the chmod program.
If the current file doesn't exist yet the buffer is saved to create it."
  (interactive "sMode (3 or 4 octal digits): ")
  (or (string-match "[0-3]?[0-7][0-7][0-7]" mode)
      (error "Invalid mode"))
  ;; make sure the file exists
  (unless (file-exists-p (buffer-file-name))
    (save-buffer))
  (set-file-modes (buffer-file-name) (string-to-number mode 8))
  (message (format "File permission set to %s" mode)))

(defun util-custom-compile (cmd custom-compile-name &optional protect)
  "Run a custom compile command in a custom buffer"
  (util-save-and-save-some-buffers)
  (let ((mybuf (current-buffer)))
    (if (get-buffer custom-compile-name)
        (kill-buffer custom-compile-name))
    (compile cmd)
    (switch-to-buffer "*compilation*")
    (rename-buffer custom-compile-name)
    (switch-to-buffer mybuf)
    ))

(defun remove-dos-eol ()
  "Do not show ^M in files containing mixed UNIX and DOS line endings."
  (interactive)
  (setq buffer-display-table (make-display-table))
  (aset buffer-display-table ?\^M []))

(defun indent-then-insert (to-insert)
  "indent then insert"
  (indent-according-to-mode) (insert to-insert))


(defun insert-then-indent (to-insert)
  "insert then indent"
  (insert to-insert) (indent-according-to-mode))

(defun insert-multiline-js-comment ()
  "/** * */"
  (interactive)
  (beginning-of-line)
  (indent-then-insert "/**\n")
  (indent-then-insert "*\n")
  (indent-then-insert "*/"))

(defun util-region-or-word ()
  "Current word or region"
  (if (not mark-active)
      (current-word)
    (buffer-substring (region-beginning) (region-end))
    ))

(defun delete-this-buffer-and-file ()
  "Removes file connected to current buffer and kills buffer."
  (interactive)
  (let ((filename (buffer-file-name))
        (buffer (current-buffer))
        (name (buffer-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (when (yes-or-no-p "Are you sure you want to remove this file? ")
        (delete-file filename)
        (kill-buffer buffer)
        (message "File '%s' successfully removed" filename)))))

(defun sort-lines-nocase ()
  "Sorts case insensitive"
  (interactive)
  (let ((sort-fold-case t))
    (call-interactively 'sort-lines)))

(defun zap-backward-whitespace ()
  "Deletes any whitespace to the left of the cursor"
  (interactive)
  (backward-char 1)
  (let ((delete-count 0))
    (while (looking-at " \\|\n")
      (progn
        (delete-forward-char 1)
        (backward-char 1)
        (setq delete-count (+ 1 delete-count))))
    delete-count))

(defun collapse-list ()
  "Take a comma separated list and remove all special newlines/whitespace/tabs around commas"
  (interactive)
  (save-excursion
    (er/mark-outside-pairs)
    (set-mark-command t)
    (backward-char)
    (save-excursion
      (if (looking-at "\\]\\|)") ;; if i'm looking at a list
          (let ((end-point (point))
                (end-char (current-char)))
            (er/mark-outside-pairs)
            (deactivate-mark)
            (forward-char)
            (setq end-point (- end-point (zap-forward-whitespace)))
            (while (<= (point) end-point)
              (progn
                (search-forward-regexp ",")
                (setq end-point (+ (- end-point (zap-forward-whitespace)) 1))
                (insert " "))))))
    (zap-backward-whitespace)))

(defun go-to-80-spot ()
  "Goes to the 80 spot of a line, or the end of the line"
  (interactive)
  (beginning-of-line)
  (let ((startpoint (point)) (endpoint) (difference))
    (end-of-line)
    (setq endpoint (point))
    (setq difference (- (- endpoint startpoint) 80))
    (if (> difference 0)
        (backward-char difference)
      )))

(defun eightyify-list ()
  "Convert a comma separated list into lines not much longer than 80 characters"
  (interactive)
  (save-excursion
    (collapse-list)
    (er/mark-outside-pairs)
    (set-mark-command t)
    (backward-char)
    (if (looking-at "\\]\\|)") ;; if i'm looking at a list
        (let ((end-point (point)))
          (er/mark-outside-pairs)
          (deactivate-mark)
          (forward-char)
          (insert-then-indent "\n")
          (go-to-80-spot)
          (while (<= (point) end-point)
            (progn
              (search-forward-regexp ",")
              (forward-char)
              (insert-then-indent "\n")
              (go-to-80-spot)))
          (search-forward-regexp "\\]\\|)")
          (backward-char)
          (insert-then-indent "\n")))))
