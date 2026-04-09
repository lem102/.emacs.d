;;; jacob-help-fns.el --- Utilities for help-fns library  -*- lexical-binding: t; -*-

;;; Commentary:
;; 

;;; Code:

(defun jacob-help-edit ()
  "Edit variable in current help buffer."
  (interactive)
  (unless (equal major-mode 'help-mode)
    (message "not in help buffer"))
  (save-excursion
    (goto-char (point-min))
    (if (search-forward "Its value is " nil "NOERROR")
        (help-fns-edit-variable)
      (message "cannot find editable variable"))))

(provide 'jacob-help-fns)

;;; jacob-help-fns.el ends here
