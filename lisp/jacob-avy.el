;;; jacob-avy.el --- Utilities for avy  -*- lexical-binding: t; -*-

;;; Commentary:
;; 

;;; Code:

(defun jacob-avy-xref (point)
  "Call `xref-find-definitions' at POINT."
  (goto-char point)
  (call-interactively #'xref-find-definitions)
  (jacob-avy-go-home))

(defun jacob-avy-kill-line (point)
  "Kill line at POINT."
  (save-excursion
    (goto-char point)
    (puni-kill-line))
  (jacob-avy-go-home))

(defun jacob-avy-copy-line (point)
  "Copy line at POINT."
  (save-excursion
    (goto-char point)
    (copy-region-as-kill (line-beginning-position) (line-end-position)))
  (jacob-avy-go-home))

(defun jacob-avy-yank-line (pt)
  "Copy sexp starting on PT."
  (jacob-avy-copy-line pt)
  (yank))

(defun jacob-avy-go-home ()
  "Return to the avy origin."
  (let ((dat (ring-ref avy-ring 0)))
    (select-frame-set-input-focus
     (window-frame (cdr dat)))
    (select-window (cdr dat))
    (goto-char (car dat))))

(defun jacob-avy-eglot-rename (pt)
  "Copy sexp starting on PT."
  (save-excursion
    (goto-char pt)
    (call-interactively #'eglot-rename))
  (jacob-avy-go-home))

(defun jacob-avy-embark (pt)
  "Call embark at PT."
  (unwind-protect
      (save-excursion
        (goto-char pt)
        (embark-act))
    (jacob-avy-go-home)))

(defun jacob-avy-mark-to-char (pt)
  "Set mark at PT."
  (activate-mark)
  (goto-char pt))

(provide 'jacob-avy)

;;; jacob-avy.el ends here
