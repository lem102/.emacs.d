;;; jacob-elisp.el --- Utilities for elisp mode  -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:

;;;###autoload
(defun jacob-elisp-eval-print-last-sexp ()
  "Run `eval-print-last-sexp', indent the result."
  (interactive)
  (save-excursion
    (eval-print-last-sexp 0))
  (save-excursion
    (forward-line)
    (indent-pp-sexp t)))

;;;###autoload
(defun jacob-elisp-move-past-close-and-reindent ()
  "Advice for `move-past-close-and-reindent'."
  (when (bolp)
    (delete-blank-lines)))

(provide 'jacob-elisp)

;;; jacob-elisp.el ends here
