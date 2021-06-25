;;; jacob-init-emacs-lisp-mode.el --- Configuration for the emacs lisp major mode.

;;; Commentary:
;; 

;;; Code:

(defun jacob-elisp-config-hook-function ()
  "Configure `emacs-lisp-mode' when hook run."
  (flymake-mode 1)
  (define-skeleton jacob-emacs-lisp-skeleton-let
    "insert let" nil
    > "(let ((" - "))" \n
    ")")

  (define-skeleton jacob-emacs-lisp-skeleton-defun
    "insert defun" nil
    > "(defun " - " ()" \n
    -2 "\"\"" \n
    ")")

  (when (boundp 'emacs-lisp-mode-abbrev-table)
    (clear-abbrev-table emacs-lisp-mode-abbrev-table))

  (define-abbrev-table 'emacs-lisp-mode-abbrev-table
    '(
      ("let" "" jacob-emacs-lisp-skeleton-let)
      ("defun" "" jacob-emacs-lisp-skeleton-defun)
      )))

(add-hook 'emacs-lisp-mode-hook 'jacob-elisp-config-hook-function)


(provide 'jacob-init-emacs-lisp-mode)

;;; jacob-init-emacs-lisp-mode.el ends here
