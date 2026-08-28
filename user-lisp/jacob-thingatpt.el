;;; jacob-thingatpt.el --- Utilities for thingatpt  -*- lexical-binding: t; -*-

;;; Commentary:
;; 

;;; Code:

;; puni-line
;; the area that `puni-soft-delete' would cover for the current line.

(defun jacob-bounds-of-puni-line-at-point ()
  "Get the bounds of the puni line at point."
  (puni-soft-delete (line-beginning-position)
                    (save-excursion
                      (forward-line)
                      (point))
                    "STRICT-SEXP"
                    'beyond
                    nil
                    nil
                    "RETURN-REGION"))

(add-to-list 'bounds-of-thing-at-point-provider-alist
             '(puni-line . jacob-bounds-of-puni-line-at-point))

;; jacob-line-content
;; from start of indentation to end of the line.

(defun jacob-bounds-of-jacob-line-content-at-point ()
  "Get the bounds of the jacob-line-content at point."
  (cons (save-excursion
          (back-to-indentation)
          (point))
        (line-end-position)))

(add-to-list 'bounds-of-thing-at-point-provider-alist
             '(jacob-line-content . jacob-bounds-of-jacob-line-content-at-point))

(provide 'jacob-thingatpt)

;;; jacob-thingatpt.el ends here
