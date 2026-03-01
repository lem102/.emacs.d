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

(provide 'jacob-apheleia)

;;; jacob-apheleia.el ends here
