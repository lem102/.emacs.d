;;; jacob-flymake.el --- Flymake utilities  -*- lexical-binding: t; -*-

;;; Commentary:
;; 

;;; Code:

;;;###autoload
(defun jacob-elisp-flymake-check-removals (report-fn &rest _args)
  "Flag `delq' `remq' and `delete'.

Recommend `remove' instead.

This is a flymake backend, hence it uses REPORT-FN to report diagnostics."
  (let ((regexp "(\\_<\\(remq\\|delq\\|delete\\)\\_>"))
    (save-excursion
      (goto-char (point-min))
      (let (diags)
        (while (re-search-forward regexp nil "NOERROR")
          (unless (nth 3 (syntax-ppss))
            (push (flymake-make-diagnostic (current-buffer)
                                           (match-beginning 1)
                                           (match-end 1)
                                           :note
                                           "Use `remove'.")
                  diags)))
        (funcall report-fn diags)))))

;;;###autoload
(defun jacob-elisp-flymake-check-custom (report-fn &rest _args)
  "Flag `use-package' `:custom' sections.

A proper `:custom' section looks like this:

:custom ((a b)
         (c d))

Flag `:custom' sections that are not formatted in this way.

This is a flymake backend, hence it uses REPORT-FN to report diagnostics."
  (let* (diags)
    (save-excursion
      (goto-char (point-min))
      (while-let ((expression (ignore-errors (read (current-buffer)))))
        (when-let ((custom-body (and (eq 'use-package (car expression))
                                     (member :custom expression))))
          (unless (and (listp (car custom-body))
                       (listp (caar custom-body)))
            (save-excursion
              (backward-sexp)
              (re-search-forward ":custom" nil "NOERROR")
              (push (flymake-make-diagnostic (current-buffer)
                                             (match-beginning 0)
                                             (match-end 0)
                                             :note
                                             "Enclose the customs in a list e.g. :custom ((a b)).")
                    diags))))))
    (funcall report-fn diags)))

(provide 'jacob-flymake)

;;; jacob-flymake.el ends here
