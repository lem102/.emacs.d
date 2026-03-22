;;; jacob-project.el --- utilities for project.el  -*- lexical-binding: t; -*-

;;; Commentary:
;; 

;;; Code:

(require 'project)

;;;###autoload
(defun jacob-dired-in-other-project ()
  "Switch to another project and open `dired' there."
  (interactive)
  (dired (project-prompt-project-dir)))

(provide 'jacob-project)

;;; jacob-project.el ends here
