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

(defun jacob-lookup-youtube ()
  "Ask for a string to search.
Search youtube for string and display in browser."
  (interactive)
  (let ((search-query (read-from-minibuffer "YouTube: ")))
    (browse-url (concat "https://www.youtube.com/results?search_query=" search-query))))
