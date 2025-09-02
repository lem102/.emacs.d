;;; jacob-xah-fly-keys-functions.el --- Functions for xah-fly-keys

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

(defvar-local jacob-forward-paragraph-function nil
  "Function to use for forward paragraph in `jacob-end-of-line'.")

(defun jacob-end-of-line ()
  "Go to content end, line end, forward paragraph."
  (interactive)
  (if (eolp)
      (if jacob-forward-paragraph-function
          (funcall jacob-forward-paragraph-function)
        (forward-paragraph))
    (let ((content-end (save-excursion
                         (when (comment-search-forward (line-end-position) "NOERROR")
                           (goto-char (match-beginning 0))
                           (skip-syntax-backward " <" (line-beginning-position))
                           (unless (= (point) (line-beginning-position))
                             (point))))))
      (if (or (null content-end)
              (= content-end (point)))
          (move-end-of-line 1)
        (goto-char content-end)))))

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

(defvar-local jacob-backspace-function nil
  "Called by `jacob-backspace' if non-nil.")

(defun jacob-backspace ()
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
    (when (= 1 (point))
      (user-error "Beginning of buffer"))
    (let ((char-class (char-syntax (char-before)))
          (f (if current-prefix-arg
                 #'delete-pair
               #'kill-sexp)))
      (unless (ignore-errors
                (funcall jacob-backspace-function f))
        (cond ((= ?\" char-class)     ; string
               (if (nth 3 (syntax-ppss))
                   (backward-char)
                 (backward-sexp))
               (funcall f))
              ((= ?\( char-class)     ; delete from start of pair
               (backward-char)
               (funcall f))
              ((= ?\) char-class)     ; delete from end of pair
               (backward-sexp)
               (funcall f))
              (t                      ; delete character
               (backward-delete-char 1)))))))

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
        (t
         (kill-line))))

(defalias 'jacob-return-macro
  (kmacro "<return>"))

(provide 'jacob-xah-fly-keys-functions)

;;; jacob-xah-fly-keys-functions.el ends here
