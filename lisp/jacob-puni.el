;;; jacob-puni.el --- Utilities for `puni'.  -*- lexical-binding: t; -*-

;;; Commentary:
;; 

;;; Code:

(defun jacob-puni-copy-line ()
  "Copy the current line, `puni' style."
  (interactive)
  (let ((point (point))
        (buffer-string (buffer-string)))
    (with-temp-buffer
      (insert buffer-string)
      (goto-char point)
      (puni-kill-line))))

(provide 'jacob-puni)

;;; jacob-puni.el ends here
