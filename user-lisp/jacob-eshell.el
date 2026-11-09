;;; jacob-eshell.el --- Eshell utilities -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'esh-mode)

(defun jacob-eshell-windows-confirm-terminate-batch-job ()
  "Type y and enter to terminate batch job after sending ^C."
  (when (not (null eshell-process-list))
    (insert "y")
    (eshell-send-input)))

(defun eshell/jacob-user-list ()
  "Print list of users on the current system."
  (shell-command-to-string "lslogins"))

(defun eshell/jacob-user-groups (&optional user)
  "Print list of groups that USER belongs to."
  (shell-command-to-string (format "id -nG %s" (if user user ""))))

(provide 'jacob-eshell)

;;; jacob-eshell.el ends here
