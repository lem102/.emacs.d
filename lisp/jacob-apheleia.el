;;; jacob-apheleia.el --- Utilities for apheleia

;;; Commentary:
;; 

;;; Code:

(defun jacob-format-play-routes-file ()
  "Format a play routes file."
  (interactive)
  (let ((regexp "^[^#+]\\([^[:space:]]+\\)\\([[:space:]]+\\)\\([^[:space:]]+\\)\\([[:space:]]+\\)"))
    (align-regexp (point-min) (point-max) regexp 2 8)
    (align-regexp (point-min) (point-max) regexp 4 8)))

(cl-defun jacob-apheleia-format-play-routes-file (&key buffer scratch callback &allow-other-keys)
  "Format a play framework routes BUFFER.
Use SCRATCH as a temporary buffer and CALLBACK to apply the
transformation."
  (with-current-buffer scratch
    (jacob-format-play-routes-file)
    (funcall callback)))

(provide 'jacob-apheleia)

;;; jacob-apheleia.el ends here
