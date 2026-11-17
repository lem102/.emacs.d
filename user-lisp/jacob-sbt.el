;;; jacob-sbt.el --- Utilities for sbt package  -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:

(require 'sbt-mode)

(defun jacob-sbt-get-project-name ()
  "Get the sbt project name."
  (with-current-buffer (sbt:buffer-name)
    (sbt-command "project")
    (accept-process-output (get-buffer-process (current-buffer))
                           1)
    (goto-char (point-min))
    (re-search-forward "\\[info\\] \\([a-z-]+\\)")
    (match-string 1)))

(provide 'jacob-sbt)

;;; jacob-sbt.el ends here
