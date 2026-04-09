;;; jacob-consult-symbol.el --- Utilities for consult-sybmol  -*- lexical-binding: t; -*-

;;; Commentary:
;; 

;;; Code:

(defun jacob-consult-symbol--default-action (sym)
  "Default action for selected symbol SYM.
Uses `customize-group' for pure custom groups, `describe-face' for
pure faces, `helpful-symbol' when available, and `describe-symbol'
as fallback.

jacob:
Patched so that `describe-keymap' is used when symbol is a keymap."
  (cond
   ((and (get sym 'group-documentation) (not (fboundp sym)) (not (boundp sym)))
    (customize-group sym))
   ((and (facep sym) (not (fboundp sym)) (not (boundp sym)))
    (describe-face sym))
   ((and (boundp sym)
         (keymapp (symbol-value sym)))
    (describe-keymap sym))
   ((fboundp 'helpful-symbol) (helpful-symbol sym))
   (t (describe-symbol sym))))

(provide 'jacob-consult-symbol)

;;; jacob-consult-symbol.el ends here
