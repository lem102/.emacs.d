;;; jacob-custom.el --- Utilities for custom  -*- lexical-binding: t; -*-

;;; Commentary:
;; 

;;; Code:

(defun jacob-format-custom-file (&rest _args)
  "Modify custom file so `package-selected-packages' is one per line."
  (with-temp-file custom-file
    (switch-to-buffer (current-buffer))
    (insert-file-contents custom-file)
    (goto-char (point-min))
    (search-forward "'(package-selected-packages")
    (forward-sexp)
    (backward-sexp)
    (down-list)
    (ignore-errors
      (while t
        (forward-sexp)
        (lisp-indent-line)
        (unless (= (char-after) (string-to-char "\n"))
          (insert "\n"))))))

(provide 'jacob-custom)

;;; jacob-custom.el ends here
