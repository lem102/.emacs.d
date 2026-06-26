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

;; Exercism

;;;###autoload
(defun jacob-project-try-exercism (dir)
  "Find exercism project in DIR."
  (when-let ((dir (locate-dominating-file dir ".exercism")))
    (list 'exercism dir)))

;;;###autoload
(cl-defmethod project-root ((project (head exercism)))
  "Get the PROJECT root for an Exercism project."
  (nth 1 project))

(provide 'jacob-project)

;;; jacob-project.el ends here
