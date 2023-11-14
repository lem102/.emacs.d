(defun jacob-config-visit ()
  "Open the init file."
  (interactive)
  (find-file "~/.emacs.d/init.el"))

(defun jacob-config-reload ()
  "Evaluate the init file."
  (interactive)
  (load-file (expand-file-name "~/.emacs.d/init.el")))

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

;; covea specific

(defun eshell/gpsugl ()
  (let* ((command (concat "git push --set-upstream origin HEAD "
                          (let* ((branch-name (with-temp-buffer
                                                (eshell-command "git symbolic-ref HEAD --short" t)
                                                (buffer-substring-no-properties (point-min) (- (point-max) 1))))
                                 (mr-key (progn
                                           (string-match (rx "mer"  "-" (+ digit))
                                                         branch-name)
                                           (match-string 0 branch-name))))
                            (concat "-o merge_request.create "
                                    "-o merge_request.remove_source_branch "
                                    (concat "-o merge_request.description=\""
                                            "[" mr-key "](" "https://coveaprodcloud.atlassian.net/browse/" mr-key ")"
                                            "\""))))))
    (with-temp-buffer
      (eshell-command command t)
      (goto-char (point-min))
      (search-forward "https")
      (browse-url-at-point))))

(defun jacob-send-mr-message ()
  (interactive)
  (let* ((gitlab-url (read-from-minibuffer "gitlab-url: "))
         (ticket-details (let* ((gitlab-mr-api "https://gitlab.tools.digital.coveahosted.co.uk/api/v4/merge_requests")
                                (mrs (jacob-web-request-helper gitlab-mr-api
                                                               "GET"
                                                               '(("PRIVATE-TOKEN" . "")) ; note to future self, never put api keys in a pulic repo :))))))
                                                               nil
                                                               nil
                                                               'json))
                                (target-mr (seq-find (lambda (mr)
                                                       (let-alist mr
                                                         (string= gitlab-url
                                                                  .web_url)))
                                                     mrs))
                                (mr-description (let-alist target-mr
                                                  .description)))
                           (string-match (rx "[" (group-n 1 (+ any)) "]"
                                             "(" (group-n 2 (+ any)) ")")
                                         mr-description)
                           (cons (match-string 1 mr-description)
                                 (match-string 2 mr-description))))
         (jira-ticket-name (car ticket-details))
         (jira-url (cdr ticket-details))
         (message (concat "Jacob Leeming: "
                          "<" gitlab-url "|MR> "
                          "for "
                          "<" jira-url "|" jira-ticket-name "> "
                          "ready for review.")))
    (jacob-web-request-helper ""
                              "POST"
                              nil
                              `((text . ,message))
                              'json-encode)))

(defvar jacob-camunda-modeler-executable
  nil "Full path to camunda modeler executable.")

(defun jacob-open-in-camunda-modeler ()
  "Attempt to open current file in camunda modeler."
  (interactive)
  (start-process "camunda-modeler" nil jacob-camunda-modeler-executable buffer-file-name))
