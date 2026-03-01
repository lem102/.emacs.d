;;; jacob-apheleia.el --- Utilities for apheleia

;;; Commentary:
;; 

;;; Code:

(defun jacob-apheleia-skip-function ()
  "Function for `apheleia-skip-functions'.
If point is in a yasnippet field or the minibuffer or region are
  active, do not format the buffer."
  (or (seq-find (lambda (overlay)
                  (overlay-get overlay 'yas--snippet))
                (overlays-at (point)))
      (minibuffer-window-active-p (car (window-list)))
      (region-active-p)))

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
