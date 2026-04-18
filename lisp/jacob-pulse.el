;;; jacob-pulse.el --- Utilities for pulse  -*- lexical-binding: t; -*-

;;; Commentary:
;; 

;;; Code:

(defun jacob-pulse--thing-at-point (thing)
  "Pulse the THING at point."
  (let ((bounds (bounds-of-thing-at-point thing)))
    (pulse-momentary-highlight-region (car bounds) (cdr bounds))))

(defun jacob-pulse-line (&rest _)
  "Pulse the line at point."
  (jacob-pulse--thing-at-point 'line))

(defun jacob-pulse-defun (&rest _)
  "Pulse the defun at point."
  (jacob-pulse--thing-at-point 'defun))

(defun jacob-pulse-previous-sexp (&rest _)
  "Pulse the sexp before point."
  (save-excursion
    (backward-sexp)
    (jacob-pulse--thing-at-point 'sexp)))

(provide 'jacob-pulse)

;;; jacob-pulse.el ends here
