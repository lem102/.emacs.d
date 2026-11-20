;;; jacob-eshell.el --- Eshell utilities -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;; TODO: a thing that detects if a shell command is running for longer than 30 seconds and if so sends a notification when it has completed.

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
