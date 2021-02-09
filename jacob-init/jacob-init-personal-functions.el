(defun jacob-insert-camel-case ()
  "ask for input, apply camel case to input and insert at point."
  (interactive)
  (let* ((input (read-string "enter words to be camel cased:"))
         (input-list (split-string input " ")))
    (insert (concat (car input-list)
                    (mapconcat 'capitalize
                               (cdr input-list)
                               "")))))

(defun jacob-count-words-region ()
  "If mark active count words in region, otherwise count words in whole buffer."
  (interactive)
  (if mark-active
      (call-interactively 'count-words-region)
    (let ((current-prefix-arg t))
      (call-interactively 'count-words-region))))

(define-key global-map (kbd "M-=") 'jacob-count-words-region)

(defun jacob-original-find-file ()
  "Uses the original file-file mechanism.
  Useful for dealing with files on other servers.
  (at least on Microsoft Windows)"
  (interactive)
  (let ((completing-read-function 'completing-read-default))
    (call-interactively 'find-file)))

(defun jacob-eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))

(defun jacob-quit-popup-window ()
  (interactive)
  (let ((loop-list (window-list))
        (window-not-found t))
    (while (and loop-list window-not-found)
      (let* ((window (car loop-list))
             (mode (jacob-buffer-mode (window-buffer window))))
        (if (or (eq mode 'help-mode)
                (eq mode 'compilation-mode)
                (eq mode 'special-mode))
            (progn
              (quit-window :window window)
              (setq window-found nil))))
      (setq loop-list (cdr loop-list)))))

(defun jacob-buffer-mode (buffer-or-string)
  "Returns the major mode associated with a buffer."
  (with-current-buffer buffer-or-string
    major-mode))

(defun jacob-config-visit ()
  (interactive)
  (dired "~/.emacs.d/jacob-init"))

(defun jacob-config-reload ()
  (interactive)
  (load-file (expand-file-name "~/.emacs.d/init.el")))

(defun jacob-org-src-block ()
  "Replacement for C-c ' in both \"org-mode\" and when editing code blocks within \"org-mode\"."
  (interactive)
  (if (bound-and-true-p org-src-mode)
      (org-edit-src-exit)
    (if (equal major-mode 'org-mode)
        (org-edit-special))))

(defun jacob-recompile-packages ()
  "Recompile all packages."
  (interactive)
  (byte-recompile-directory package-user-dir nil 'force))

(defun jacob-split-window-below-select-new ()
  "Splits current window vertically, then switch to new window."
  (interactive)
  (split-window-below)
  (other-window 1))

(defun jacob-split-window-right-select-new ()
  "Splits current window horizontally, then switch to new window."
  (interactive)
  (split-window-right)
  (other-window 1))

(load-file (expand-file-name "~/.emacs.d/myLisp/jacob-long-time.el"))

(defun jacob-display-time ()
  "Display the current date and time in the echo area."
  (interactive)
  (message (concat (format-time-string "%A the %e")
                   (jacob-day-suffix (string-to-number (format-time-string "%e")))
                   (format-time-string " of %B, the year of our Lord %Y, ")
                   "at "
                   (jacob-long-time (string-to-number (format-time-string "%H"))
                                    (string-to-number (format-time-string "%M")))
                   ".")))

(defun jacob-xah-insert-bracket-pair (@left-bracket @right-bracket)
  "Heavily simplified version of Xah's excellent function. 
My usecases differ to his, so I have removed a vast amount of the functionality. 
Now, this function will insert a pair, or wrap the region if it is active.

Original Version can be found here:
URL `http://ergoemacs.org/emacs/elisp_insert_brackets_by_pair.html'"

  (if (use-region-p)
      (let (($p1 (region-beginning))
            ($p2 (region-end)))
        (goto-char $p2)
        (insert @right-bracket)
        (goto-char $p1)
        (insert @left-bracket)
        (goto-char (+ $p2 2)))
    (let ($p1 $p2)
      (setq $p1 (point) $p2 (point))
      (insert @left-bracket @right-bracket)
      (search-backward @right-bracket))))

(defun jacob-xah-insert-paren ()
  (interactive)
  (jacob-xah-insert-bracket-pair "(" ")"))

(defun jacob-xah-insert-square-bracket ()
  (interactive)
  (jacob-xah-insert-bracket-pair "[" "]"))

(defun jacob-xah-insert-brace ()
  (interactive)
  (jacob-xah-insert-bracket-pair "{" "}"))

(defun jacob-xah-insert-ascii-double-quote ()
  (interactive)
  (jacob-xah-insert-bracket-pair "\"" "\""))

(defun jacob-xah-insert-ascii-single-quote ()
  (interactive)
  (jacob-xah-insert-bracket-pair "'" "'"))

(defun jacob-insert-plus ()
  (interactive)
  (insert "+"))

(defun jacob-insert-equals ()
  (interactive)
  (insert "="))

(defun jacob-insert-apostrophe ()
  (interactive)
  (insert "'"))

(defun jacob-insert-at ()
  (interactive)
  (insert "@"))

(defun jacob-insert-tilde ()
  (interactive)
  (insert "~"))

(defun jacob-insert-hash ()
  (interactive)
  (insert "#"))

(fset 'jacob-enter-kmacro
      [return])

(fset 'jacob-backspace-kmacro
      [?f backspace home])
