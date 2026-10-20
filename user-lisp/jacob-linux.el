;;; jacob-linux.el --- Utilities for Linux.  -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:

;;;###autoload
(defun jacob-install-desktop-files ()
  "Install custom desktop files into user desktop file directory."
  (interactive)
  (let* ((source (file-name-concat user-emacs-directory "resources"))
         (target "~/.local/share/applications"))
    (copy-file (file-name-concat source "emacsclient.desktop")
               (file-name-concat target "emacsclient.desktop")
               0 ; pass an integer so that `copy-file' prompts the user
               )))

(provide 'jacob-linux)

;;; jacob-linux.el ends here
