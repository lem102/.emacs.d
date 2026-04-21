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

(provide 'jacob-org)

;;; jacob-org.el ends here
