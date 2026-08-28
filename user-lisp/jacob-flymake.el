;;; jacob-flymake.el --- Flymake utilities  -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:

(require 'jacob-utils)

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
          (unless (jacob-in-text-p)
            (push (flymake-make-diagnostic (current-buffer)
                                           (match-beginning 1)
                                           (match-end 1)
                                           :note
                                           "Use `remove'.")
                  diags)))
        (funcall report-fn diags)))))

;;;###autoload
(defun jacob-flymake-use-package (report-fn &rest _args)
  "Flag style issues with `use-package' declarations.

This is a flymake backend, hence it uses REPORT-FN to report diagnostics."
  (let* (diags)
    (save-excursion
      (goto-char (point-min))
      (while-let ((sexp (ignore-errors (read (current-buffer)))))
        (when (eq 'use-package (car sexp))
          (let* ((tree (named-let go ((declaration sexp) (tree nil))
                         (cond ((null declaration)
                                (seq-map #'reverse (reverse tree)))
                               ((eq 'use-package (car declaration))
                                (go (cddr declaration)
                                    tree))
                               ((keywordp (car declaration))
                                (go (cdr declaration)
                                    (cons (list (car declaration))
                                          tree)))
                               (t
                                (go (cdr declaration)
                                    (cons (cons (car declaration)
                                                (car tree))
                                          (cdr tree))))))))
            (when (seq-some (lambda (branch)
                              (and (eq :custom (car branch))
                                   (or (symbolp (caadr branch))
                                       (not (= 2 (length branch))))))
                            tree)
              (save-excursion
                (backward-sexp)
                (re-search-forward ":custom" nil "NOERROR")
                (push (flymake-make-diagnostic (current-buffer)
                                               (match-beginning 0)
                                               (match-end 0)
                                               :note
                                               "Enclose the customs in a list e.g. :custom ((a b)).")
                      diags)))
            (when (seq-some (lambda (branch)
                              (and (eq :hook (car branch))
                                   (or (symbolp (caadr branch))
                                       (not (= 2 (length branch))))))
                            tree)
              (save-excursion
                (backward-sexp)
                (re-search-forward ":hook" nil "NOERROR")
                (push (flymake-make-diagnostic (current-buffer)
                                               (match-beginning 0)
                                               (match-end 0)
                                               :note
                                               "Enclose the hooks in a list e.g. :hook ((a . b)).")
                      diags)))
            (let* ((keywords (seq-map #'car tree)))
              (when (and (member :defer keywords)
                         (or (member :hook keywords)
                             (member :bind keywords)
                             (member :mode keywords)))
                (save-excursion
                  (backward-sexp)
                  (re-search-forward ":defer" nil "NOERROR")
                  (push (flymake-make-diagnostic (current-buffer)
                                                 (match-beginning 0)
                                                 (match-end 0)
                                                 :note
                                                 "Unnecessary :defer keyword.")
                        diags))))
            (when (seq-some (lambda (branch)
                              (and (eq :functions (car branch))
                                   (or (symbolp (cadr branch))
                                       (not (= 2 (length branch))))))
                            tree)
              (save-excursion
                (backward-sexp)
                (re-search-forward ":functions" nil "NOERROR")
                (push (flymake-make-diagnostic (current-buffer)
                                               (match-beginning 0)
                                               (match-end 0)
                                               :note
                                               ":functions should be a list.")
                      diags)))))))
    (funcall report-fn diags)))

(provide 'jacob-flymake)

;;; jacob-flymake.el ends here
