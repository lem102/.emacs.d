;;; jacob-org.el --- Utilities for org  -*- lexical-binding: t; -*-

;;; Commentary:
;; 

;;; Code:

(defun jacob-org-babel-tangle-delete-whitespace ()
  "Get rid of the whitespace at the end of the buffer."
  (goto-char (point-max))
  (delete-trailing-whitespace)
  (backward-delete-char 1)
  (save-buffer))

(defun jacob-org-jira-follow (issue _)
  "Open the jira ISSUE."
  (browse-url (file-name-concat jacob-jira-url issue)))

(defun jacob-org-project-follow (project-name _)
  "Open the project corresponding to PROJECT-NAME."
  (let* ((project-directory (seq-find (lambda (project)
                                        (string= project-name
                                                 (file-name-nondirectory (directory-file-name project))))
                                      (mapcar #'car project--list)))
         (project (project-current nil project-directory)))
    (dired (project-root project))))

(provide 'jacob-org)

;;; jacob-org.el ends here
