;;; jacob-utils.el --- Utilities  -*- lexical-binding: t; -*-

;;; Commentary:
;; Useful functions that can be used across my init files

;;; Code:

(defun jacob-in-text-p (&optional position)
  "Return non-nil if POSITION is in text, e.g. a string or comment."
  (or (jacob-in-string-p position)
      (jacob-in-comment-p position)))

(defun jacob-in-string-p (&optional position)
  "Return non-nil if POSITION is in a string."
  (save-excursion
    (nth 3 (syntax-ppss position))))

(defun jacob-in-comment-p (&optional position)
  "Return non-nil if POSITION is in a comment."
  (save-excursion
    (nth 4 (syntax-ppss position))))

(defun jacob-in-code-p (&optional position)
  "Return non-nil if POSITION is in code, as apposed to being in text."
  (not (jacob-point-in-text-p position)))

(provide 'jacob-utils)

;;; jacob-utils.el ends here
