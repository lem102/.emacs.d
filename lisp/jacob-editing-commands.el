;;; jacob-editing-commands.el --- Editing commands

;;; Commentary:
;;

;;; Code:

(defun jacob-xfk-local-key (key command)
  "Bind KEY buffer locally to COMMAND in xfk command mode."
  (let ((existing-command (keymap-lookup xah-fly-command-map key nil "NO-REMAP")))
    (unless existing-command
      (user-error "%s is not bound to a key in `xah-fly-command-map'" key))
    (keymap-local-set (format "<remap> <%s>" existing-command)
                      command)))

(defmacro jacob-xfk-bind-for-mode (mode &rest bindings)
  "Use BINDINGS when in a certain MODE."
  (unless (cl-evenp (length bindings))
    (user-error "`jacob-xfk-bind-for-mode' %s bindings is not a plist"
                mode))
  (let* ((hook (intern (concat (symbol-name mode) "-hook")))
         (hook-function (intern (concat "jacob-" (symbol-name hook) "-function")))
         (binding-alist (seq-reduce (lambda (p c)
                                      "Convert the plist into an alist"
                                      (cond ((and (caar p)
                                                  (null (cdar p)))
                                             (cons (cons (caar p) c)
                                                   (cdr p)))
                                            (t (cons (cons c nil)
                                                     p))))
                                    bindings
                                    '()))
         (hook-function-body (seq-map (lambda (pair)
                                        "PAIR is `'(key command)', return code to bind key to command locally."
                                        (let ((key (car pair))
                                              (command (cdr pair)))
                                          `(let ((existing-command (keymap-lookup xah-fly-command-map
                                                                                  ,key
                                                                                  nil
                                                                                  "NO-REMAP")))
                                             (unless existing-command
                                               (user-error "%s is not bound to a key in `xah-fly-command-map'"
                                                           ,key))
                                             (keymap-local-set (format "<remap> <%s>"
                                                                       existing-command)
                                                               ,command))))
                                      binding-alist)))
    `(progn
       (defun ,hook-function ()
         ,(format "Auto-generated hook function for `%s'." (symbol-name hook))
         ,@hook-function-body)
       (add-hook ',hook #',hook-function))))

(defvar-local jacob-backward-paragraph-function nil
  "Function to use for backward paragraph in `jacob-beginning-of-line'.")

(defun jacob-beginning-of-line ()
  "Go to indentation, line start, backward paragraph."
  (interactive)
  (cond ((bolp)
         (if jacob-backward-paragraph-function
             (funcall jacob-backward-paragraph-function)
           (backward-paragraph)))
        ((= (save-excursion
              (back-to-indentation)
              (point))
            (point))
         (move-beginning-of-line 1))
        (t
         (back-to-indentation))))

(defvar-local jacob-delete-backwards-function nil
  "Called by `jacob-delete-backwards' if non-nil.")

(defun jacob-delete-backwards ()
  "DWIM backspace command.

  If character to the left is a pair character as determined by
  `insert-pair-alist', kill from the pair to its match. If the prefix
  argument is provided, only delete the pair characters.

  If the character to the left of the cursor is whitespace, delete all
  the whitespace backward from point to the first non whitespace
  character."
  (interactive)
  (undo-boundary)
  (if (region-active-p)
      (delete-active-region)
    (push-mark)
    (when (= 1 (point))
      (user-error "Beginning of buffer"))
    (let ((char-class (char-syntax (char-before)))
          (delete-function (if current-prefix-arg
                               #'delete-pair
                             #'kill-sexp)))
      (unless (ignore-errors
                (funcall jacob-delete-backwards-function delete-function))
        (cond ((= ?\" char-class)     ; string
               (if (nth 3 (syntax-ppss))
                   (progn
                     (backward-char)
                     (save-excursion
                       (forward-sexp)
                       (push-mark)))
                 (backward-sexp))
               (funcall delete-function))
              ((= ?\( char-class)     ; delete from start of pair
               (backward-char)
               (save-excursion
                 (forward-sexp)
                 (push-mark))
               (funcall delete-function))
              ((= ?\) char-class)     ; delete from end of pair
               (backward-sexp)
               (funcall delete-function))
              (t                      ; delete character
               (backward-delete-char-untabify 1)))))))

(defun jacob-kill-line ()
  "If region is active, kill it.  Otherwise:

  If point is at the beginning of the line, kill the whole line.

  If point is at the end of the line, kill until the beginning of the line.

  Otherwise, kill from point to the end of the line."
  (interactive)
  (cond ((region-active-p)
         (call-interactively #'kill-region))
        ((bolp)
         (kill-whole-line))
        ((eolp)
         (kill-line 0))
        ((and (= ?\( (char-syntax (char-before)))
              (= ?\) (char-syntax (char-after))))
         (kill-line))
        ((= ?\) (char-syntax (char-after)))
         (puni-backward-kill-line))
        (t
         (puni-kill-line))))

(defalias 'jacob-return-macro
  (kmacro "<return>"))

(defun jacob-mark-line ()
  "Mark the current line."
  (interactive)
  (if (region-active-p)
      (end-of-line 2)
    (push-mark (line-beginning-position) "NOMSG" "ACTIVATE")
    (end-of-line)))

(defun jacob-mark-paragraph ()
  "TODO: write documentation."
  (interactive)
  (unless (region-active-p)
    (backward-paragraph)
    (push-mark (point) "NOMSG" "ACTIVATE"))
  (forward-paragraph))

(defun jacob-copy-line-or-region ()
  "Copy current line or region."
  (interactive)
  (cond ((region-active-p)
         (kill-ring-save nil nil "REGION"))
        ((eq last-command this-command)
         (kill-append (concat "\n"
                              (buffer-substring (line-beginning-position)
                                                (line-end-position)))
                      nil)
         (forward-line))
        (t
         (kill-ring-save (line-beginning-position) (line-end-position))
         (forward-line))))

(defun jacob-delete-whitespace ()
  "Delete different parts of whitespace on repeated invocations."
  (interactive)
  (if (string-blank-p (buffer-substring (line-beginning-position)
                                        (line-end-position)))
      (delete-blank-lines)
    (join-line)))

(defun xah-shrink-whitespaces ()
  "Remove whitespaces around cursor.

Shrink neighboring spaces, then newlines, then spaces again, leaving
one space or newline at each step, till no more white space.

URL `http://xahlee.info/emacs/emacs/emacs_shrink_whitespace.html'
Created: 2014-10-21
Version: 2023-07-12"
  (interactive)
  (let ((xeol-count 0)
        (xp0 (point))
        xbeg  ; whitespace begin
        xend  ; whitespace end
        (xcharBefore (char-before))
        (xcharAfter (char-after))
        xspace-neighbor-p)
    (setq xspace-neighbor-p (or (eq xcharBefore 32) (eq xcharBefore 9) (eq xcharAfter 32) (eq xcharAfter 9)))
    (skip-chars-backward " \n\t　")
    (setq xbeg (point))
    (goto-char xp0)
    (skip-chars-forward " \n\t　")
    (setq xend (point))
    (goto-char xbeg)
    (while (search-forward "\n" xend t)
      (setq xeol-count (1+ xeol-count)))
    (goto-char xp0)
    (cond
     ((eq xeol-count 0)
      (if (> (- xend xbeg) 1)
          (progn
            (delete-horizontal-space) (insert " "))
        (progn (delete-horizontal-space))))
     ((eq xeol-count 1)
      (if xspace-neighbor-p
          (delete-horizontal-space)
        (progn (delete-space--internal "\n" nil) (insert " "))))
     ((eq xeol-count 2)
      (if xspace-neighbor-p
          (delete-horizontal-space)
        (progn
          (delete-space--internal "\n" nil)
          (insert "\n"))))
     ((> xeol-count 2)
      (if xspace-neighbor-p
          (delete-horizontal-space)
        (progn
          (goto-char xend)
          (search-backward "\n")
          (delete-region xbeg (point))
          (insert "\n"))))
     (t (progn
          (message "nothing done. logic error 40873. shouldn't reach here"))))))

(defun xah-comment-dwim ()
  "Toggle comment in programing language code.

Like `comment-dwim', but toggle comment if cursor is not at end of
line.

If cursor is at end of line, either add comment at the line end or
move cursor to start of line end comment. call again to comment out
whole line.

URL `http://xahlee.info/emacs/emacs/emacs_toggle_comment_by_line.html'
Created: 2016-10-25
Version: 2023-07-10"
  (interactive)
  (if (region-active-p)
      (comment-dwim nil)
    (let ((xbegin (line-beginning-position))
          (xend (line-end-position)))
      (if (eq xbegin xend)
          (progn
            (comment-dwim nil))
        (if (eq (point) xend)
            (progn
              (comment-dwim nil))
          (progn
            (comment-or-uncomment-region xbegin xend)
            (forward-line)))))))

(defun jacob-end-of-line ()
  "Go to content end, line end, forward paragraph."
  (interactive)
  (if (eolp)
      (forward-paragraph)
    (let ((content-end (save-excursion
                         (when (condition-case error
                                   (comment-search-forward (line-end-position) "NOERROR")
                                 (beginning-of-buffer nil))
                           (goto-char (match-beginning 0))
                           (skip-syntax-backward " <" (line-beginning-position))
                           (unless (= (point) (line-beginning-position))
                             (point))))))
      (if (or (null content-end)
              (= content-end (point)))
          (move-end-of-line 1)
        (goto-char content-end)))))

(defun jacob-copy-buffer ()
  "Copy the text of the current buffer."
  (interactive)
  (kill-new (buffer-string)))

(defun jacob-toggle-word-case ()
  "Toggle the case of the word at point."
  (interactive)
  (let* ((case-fold-search nil)
         (word (thing-at-point 'word)))
    (save-excursion
      (backward-word)
      (funcall (cond ((not (string-match-p "[[:upper:]]" word))
                      #'capitalize-word)
                     ((string-match-p "^[[:upper:]][^[:upper:]]*$" word)
                      #'upcase-word)
                     (t
                      #'downcase-word))
               1))))

(defun xah-toggle-letter-case ()
  "Toggle the letter case of current word or selection.
Always cycle in this order: Init Caps, ALL CAPS, all lower.

URL `http://xahlee.info/emacs/emacs/emacs_toggle_letter_case.html'
Created: 2020-06-26
Version: 2024-06-17"
  (interactive)
  (let ((deactivate-mark nil) xbeg xend)
    (if (region-active-p)
        (setq xbeg (region-beginning) xend (region-end))
      (save-excursion
        (skip-chars-backward "[:alnum:]")
        (setq xbeg (point))
        (skip-chars-forward "[:alnum:]")
        (setq xend (point))))
    (when (not (eq last-command this-command))
      (put this-command 'state 0))
    (cond
     ((equal 0 (get this-command 'state))
      (upcase-initials-region xbeg xend)
      (put this-command 'state 1))
     ((equal 1 (get this-command 'state))
      (upcase-region xbeg xend)
      (put this-command 'state 2))
     ((equal 2 (get this-command 'state))
      (downcase-region xbeg xend)
      (put this-command 'state 0)))))

(defun jacob-toggle-previous-letter-case ()
  "Toggle the case of the previous letter."
  (interactive)
  (let ((case-fold-search nil)
        (f (if (string-match-p "[[:upper:]]" (char-to-string (char-before)))
               #'downcase-region
             #'upcase-region)))
    (funcall f (1- (point)) (point))))

(defun xah-toggle-previous-letter-case ()
  "Toggle the letter case of the letter to the left of cursor.

URL `http://xahlee.info/emacs/emacs/emacs_toggle_letter_case.html'
Created: 2015-12-22
Version: 2023-11-14"
  (interactive)
  (let ((case-fold-search nil))
    (left-char 1)
    (cond
     ((looking-at "[[:lower:]]") (upcase-region (point) (1+ (point))))
     ((looking-at "[[:upper:]]") (downcase-region (point) (1+ (point)))))
    (right-char)))

(provide 'jacob-editing-commands)

;;; jacob-editing-commands.el ends here
