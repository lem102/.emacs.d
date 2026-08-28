;;; jacob-diff.el --- Extensions to diff functionality  -*- lexical-binding: t; -*-

;;; Commentary:
;; 

;;; Code:

;;;###autoload
(defun jacob-diff-unapply-hunk ()
  "In diff mode, unapply the hunk at point."
  (interactive)
  (call-interactively #'diff-apply-hunk "REVERSE"))

(provide 'jacob-diff)

;;; jacob-diff.el ends here
