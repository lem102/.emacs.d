;;; jacob-personal-functions.el --- my personal functions

;;; Commentary:
;; 

;;; Code:

(defun jacob-alist-to-form-data (alist)
  "Convert ALIST to form-data for http request."
  (mapconcat (lambda (x)
               (concat (car x) "=" (cdr x)))
             alist
             "&"))

(defun jacob-format-xml ()
  "Format xml on current line."
  (insert (let ((input (prog1
                           (buffer-substring-no-properties (line-beginning-position) (line-end-position))
                         (delete-region (line-beginning-position) (line-end-position)))))
            (with-temp-buffer
              (insert input)
              (goto-char (point-min))
              (while (search-forward-regexp "\>[ \\t]*\<" nil t)
                (backward-char) (insert "\n"))
              (nxml-mode)
              (indent-region (point-min) (point-max))
              (buffer-substring-no-properties (point-min) (point-max))))))

(defun jacob-web-request-helper (method url &optional headers data data-format-function)
  "Helper function for making web requests.
METHOD, HEADERS and DATA are for the corresponding url-request variables.
URL is the address to send the request to.
Returns a string containing the response.

DATA-FORMAT-FUNCTION is a function that takes one argument and returns
a string."
  (require 'json)
  (with-current-buffer (let ((url-request-method method)
                             (url-request-extra-headers headers)
                             (url-request-data (if (null data-format-function)
                                                   data
                                                 (funcall data-format-function data))))
                         (url-retrieve-synchronously url))
    (beginning-of-buffer)
    (when (search-forward-regexp "Content-Type: application/[a-z+]*json" nil t)
      (search-forward "\n\n" nil t)
      (json-reformat-region (point) (point-max)))
    (when (search-forward-regexp "Content-Type:.*xml" nil t)
      (search-forward "\n\n" nil t)
      (jacob-format-xml))
    (buffer-string)))

(defun jacob-open-in-camunda-modeler ()
  "Attempt to open current file in camunda modeler."
  (interactive)
  (start-process "camunda-modeler" nil jacob-camunda-modeler-executable buffer-file-name))

(defun jacob-goto-pi ()
  "Connect to raspberry pi."
  (interactive)
  (find-file jacob-raspberry-pi-connection-string))

(defun jacob-toggle-modeline ()
  "Toggle visibility of modeline."
  (interactive)
  (setq mode-line-format (if (null mode-line-format)
                             jacob-mode-line-format
                           nil)))

(defun jacob-start-timer ()
  "Run a 25 min timer."
  (interactive)
  (require 'org-timer)
  (org-timer-set-timer "25"))

(defun jacob-new-tab ()
  "Make a new tab and give it a name."
  (interactive)
  (tab-bar-new-tab)
  (call-interactively 'tab-rename))

(jacob-is-installed 'consult
  (defun jacob-project-search ()
    "If current project is a git project, use consult git grep, otherwise use consult grep."
    (interactive)
    (if (vc-find-root default-directory ".git")
        (consult-git-grep)
      (consult-grep))))

(defun jacob-async-shell-command (command)
  "Wrapper command for `async-shell-command'."
  (interactive
   (list
    (read-shell-command (if shell-command-prompt-show-cwd
                            (format-message "Async shell command in `%s': "
                                            (abbreviate-file-name
                                             default-directory))
                          "Async shell command: ")
                        nil nil
			            (let ((filename
			                   (cond
				                (buffer-file-name)
				                ((eq major-mode 'dired-mode)
				                 (dired-get-filename nil t)))))
			              (and filename (file-relative-name filename))))))
  (async-shell-command command
                       (concat "* " default-directory " " command " *")))

(defvar jacob-format-words-2-style-and-start nil
  "Pair of currently selected style and starting point.
If nil, means you havent used the command for the first time yet.")

(defun jacob-format-words-2 ()
  "Command for formating words into identifiers when writing code.

On first use, ask for formatting style (e.g. kebab, camel, etc).
Store current point, and selected style.

On second use, format from current point to point saved from first use
in the selected style also from first use."
  (interactive)
  (if jacob-format-words-2-style-and-start
      (let* ((style (car jacob-format-words-2-style-and-start))
             (start-point (cdr jacob-format-words-2-style-and-start))
             (words (split-string (delete-and-extract-region start-point (point)) " ")))
        (insert (pcase style
                  ("camelCase" (concat (car words)
                                       (mapconcat 'capitalize (cdr words) "")))
                  ("PascalCase" (mapconcat 'capitalize words ""))
                  ("kebab-case" (string-join words "-"))
                  ("snake_case" (string-join words "_"))
                  ("SCREAMING_SNAKE_CASE" (mapconcat 'upcase words "_"))))
        (setq jacob-format-words-2-style-and-start nil))
    (let ((style-choice (completing-read "choose: " '("camelCase" "PascalCase" "kebab-case" "snake_case" "SCREAMING_SNAKE_CASE"))))
      (message (concat style-choice " selected"))
      (setq jacob-format-words-2-style-and-start (cons style-choice
                                                       (point))))))

(defvar jacob-format-words-3-style-and-start nil
  "Pair of currently selected style and starting point.
If nil, means you havent used the command for the first time yet.")

(defun jacob-format-words-3 ()
  "Command for formating words into identifiers when writing code.

On first use, ask for formatting style (e.g. kebab, camel,
etc).  Format one word backwards in selected style and store the style
and the position of point after formatting, return point to where it
was when command called.

On consecutive use, apply stored formatting to word before stored
point."
  (interactive)

  (undo-boundary)
  
  (unless (eq last-command this-command)
    (setq jacob-format-words-3-style-and-start (cons (read-char-from-minibuffer "select style: " '(?c ?p ?k ?s ?S))
                                                     (point))))
  
  (save-excursion
    (let* ((style (car jacob-format-words-3-style-and-start))
           (format-position (cdr jacob-format-words-3-style-and-start))
           (bounds (progn
                     (goto-char format-position)
                     (bounds-of-thing-at-point 'word)))
           (start (car bounds))
           (end (cdr bounds)))
      (pcase style
        (?c (progn
              (backward-word)
              (capitalize-word 1)
              (backward-word)
              (backward-delete-char 1)))
        (?p (progn
              (backward-word)
              (capitalize-word 1)
              (backward-word)
              (backward-delete-char 1)
              (backward-word)
              (capitalize-word 1)))
        (?k (progn
              (backward-word)
              (backward-delete-char 1)
              (insert-char ?-)
              (backward-char)))
        (?s (progn
              (backward-word)
              (backward-delete-char 1)
              (insert-char ?_)
              (backward-char)))
        (?S (progn
              (backward-word)
              (upcase-word 1)
              (backward-word)
              (backward-delete-char 1)
              (insert-char ?_)
              (backward-char)
              (backward-word)
              (upcase-word 1))))

      (setq jacob-format-words-3-style-and-start (cons style (point))))))

(defun jacob-count-words-region ()
  "If mark active count words in region, otherwise count words in whole buffer."
  (interactive)
  (if mark-active
      (call-interactively 'count-words-region)
    (let ((current-prefix-arg t))
      (call-interactively 'count-words-region))))

(define-key global-map (kbd "M-=") 'jacob-count-words-region)

(defun jacob-eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))

(defun jacob-config-visit ()
  "Open the init file."
  (interactive)
  (find-file "~/.emacs.d/init.el"))

(defun jacob-config-reload ()
  "Evaluate the init file."
  (interactive)
  (load-file (expand-file-name "~/.emacs.d/init.el")))

(defun jacob-config-update ()
  "Download latest version of config from git."
  (interactive)
  (shell-command "git -C ~/.emacs.d pull"))

(jacob-is-installed 'restart-emacs
  (defun jacob-config-update-then-restart ()
    "Update config then restart."
    (interactive)
    (jacob-config-update)
    (restart-emacs)))

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

(defun jacob-insert-exclamation-mark ()
  (interactive)
  (insert "!"))

(defun jacob-insert-pound-sign ()
  (interactive)
  (insert "£"))

(defun jacob-insert-dollar-sign ()
  (interactive)
  (insert "$"))

(defun jacob-insert-percentage-sign ()
  (interactive)
  (insert "%"))

(defun jacob-insert-caret ()
  (interactive)
  (insert "^"))

(defun jacob-insert-ampersand ()
  (interactive)
  (insert "&"))

(defun jacob-insert-asterisk ()
  (interactive)
  (insert "*"))

(fset 'jacob-enter-kmacro
      [return])

(fset 'jacob-backspace-kmacro
      [?f backspace home])

(defun jacob-matlab-matrix-to-latex (matrix-start matrix-end)
  (interactive (if (use-region-p)
                   (list (region-beginning) (region-end))
                 (let (region-start
                       region-end)
                   (search-backward "[")
                   (setq region-start (point))
                   (search-forward "]")
                   (setq region-end (point))
                   (list region-start region-end))))
  (save-excursion
    (save-restriction
      (narrow-to-region matrix-start matrix-end)

      (progn
        (goto-char (point-min))
        (while (search-forward-regexp "[[:space:]]+" nil t)
          (replace-match " ")))
      
      (dolist (pair (list (quote ("[ " "["))
                          (quote ("[" "\\\\jbmat{"))
                          (quote (" ]" "]"))
                          (quote ("]" "}"))
                          (quote ("; " ";"))
                          (quote (" " " & "))
                          (quote (";" " \\\\\\\\ "))))
        (progn
          (goto-char (point-min))
          (while (search-forward (car pair) nil t)
            (replace-match (car (last pair)))))))))

(defun jacob-system-shutdown ()
  "Prompts for yes/no input.

If user inputs yes, system is shutdown.  Otherwise, nothing happens."
  (interactive)
  (if (yes-or-no-p "Shutdown system?")
      (shell-command "pwsh -Command Stop-Computer")))

(defun jacob-eshell-dwim ()
  "Call different eshell commands depending on the context.

If the current buffer is an eshell buffer, call the `eshell'
command with universal argument.  If the current buffer is under
version control, call `project-eshell' instead."
  (interactive)
  (let ((current-prefix-arg (eq major-mode 'eshell-mode))
        (eshell-command (if (eq 'Git (vc-backend (buffer-file-name)))
                            'project-eshell
                          'eshell)))
    (call-interactively eshell-command)))

(defun josh-kill-process-on-port ()
  "Ask for a port, kill process on that port.  For powershell."
  (interactive)
  (shell-command (concat "powershell.exe -File %home%\\Downloads\\Jacob.ps1 -localPort " (read-from-minibuffer "port: "))))

(defun jacob-lookup-youtube ()
  "Ask for a string to search.
Search youtube for string and display in browser."
  (interactive)
  (let ((search-query (read-from-minibuffer "YouTube: ")))
    (browse-url (concat "https://www.youtube.com/results?search_query=" search-query))))

(defun jacob-lookup-wikipedia ()
  "Ask for a string to search.
Search youtube for string and display in browser."
  (interactive)
  (let ((search-query (read-from-minibuffer "Wikipedia: ")))
    (browse-url (concat "https://en.wikipedia.org/wiki/" search-query))))

(defun jacob-bookmark-jump-to-url (bookmark)
  "Open link stored in the filename property of BOOKMARK in browser."
  (browse-url (cdr (assoc 'filename (cdr bookmark)))))

(defun jacob-swap-visible-buffers ()
  "If two windows in current frame, swap their buffers.
Otherwise, display error message."
  (interactive)
  (if (length= (window-list) 2)
      (let* ((current-window (car (window-list)))
             (other-window (cadr (window-list)))
             (current-buffer (window-buffer current-window))
             (other-buffer (window-buffer other-window)))
        (set-window-buffer current-window other-buffer)
        (set-window-buffer other-window current-buffer)
        (other-window 1))
    (message "More/less than 2 windows in frame.")))



;; voice commands

(defun jacob-recenter-top ()
  (interactive)
  (recenter 5))

(defun jacob-recenter-centre ()
  (interactive)
  (recenter))

(defun jacob-recenter-bottom ()
  (interactive)
  (recenter -5))

(defun jacob-move-to-window-line-top ()
  (interactive)
  (move-to-window-line 5))

(defun jacob-move-to-window-line-centre ()
  (interactive)
  (move-to-window-line nil))

(defun jacob-move-to-window-line-bottom ()
  (interactive)
  (move-to-window-line -5))

(defun jacob-voice-mark-command ()
  (interactive)
  (if (region-active-p)
      (er/expand-region 1)
    (set-mark (point))))

(defun jacob-switch-to-previous-buffer ()
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

(defun jacob-xah-define-key (major-mode-keymap key command)
  "In MAJOR-MODE-KEYMAP bind KEY to COMMAND only when in xfk command mode."
  (define-key major-mode-keymap (vector 'remap (lookup-key xah-fly-command-map key)) command))

(provide 'jacob-personal-functions)

;;; jacob-personal-functions.el ends here
