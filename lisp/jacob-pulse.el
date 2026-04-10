;;; jacob-pulse.el --- Utilities for pulse library  -*- lexical-binding: t; -*-

;;; Commentary:
;; 

;;; Code:

(defun jacob-pulse-line (&rest _)
  "Pulse the current line."
  (pulse-momentary-highlight-region (save-excursion
                                      (back-to-indentation)
                                      (point))
                                    (line-end-position)))

(defun jacob-pulse-defun (&rest _)
  "Pulse the defun at point."
  (let ((bounds (bounds-of-thing-at-point 'defun)))
    (pulse-momentary-highlight-region (car bounds) (cdr bounds))))

(provide 'jacob-pulse)

;;; jacob-pulse.el ends here
