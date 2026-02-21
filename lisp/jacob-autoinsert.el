;;; jacob-autoinsert.el --- Support library for `autoinsert'  -*- lexical-binding: t; -*-

;;; Commentary:
;; 

;;; Code:

(defun jacob-define-auto-insert (condition action)
  "Set up an auto insert idempotently.

CONDITION and ACTION are as in `define-auto-insert'."
  (when (assoc condition auto-insert-alist)
    (setopt auto-insert-alist (seq-remove (lambda (ai)
                                            (equal condition (car ai)))
                                          auto-insert-alist)))
  (define-auto-insert condition action))

(provide 'jacob-autoinsert)

;;; jacob-autoinsert.el ends here
