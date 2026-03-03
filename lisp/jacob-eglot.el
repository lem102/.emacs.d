;;; jacob-eglot.el --- Utilities for `eglot'  -*- lexical-binding: t; -*-

;;; Commentary:
;; 

;;; Code:

(defun jacob-eglot-yank ()
  "Yank text into the buffer, attempt to fix any import issues."
  (interactive)
  (let ((start (point)))
    (cl-flet ((handle-code-actions (_connection origin &key kind)
                (when (and (eq origin 'server)
                           (eq kind 'notification))
                  (eglot-code-actions start (point) nil "INTERACTIVE")
                  (remove-hook 'jsonrpc-event-hook #'handle-code-actions))))
      (yank)
      (save-buffer)
      (add-hook 'jsonrpc-event-hook #'handle-code-actions))))

(defun jacob-remove-ret-character-from-buffer (&rest _)
  "Remove all occurances of ^M from the buffer.

    Useful for deleting ^M after `eglot-code-actions'."
  ;; TODO: Review if needed.
  (save-excursion
    (goto-char (point-min))
    (while (search-forward (char-to-string 13) nil t)
      (replace-match ""))))

(defun jacob-eglot-xref-backend ()
  "Custom eglot xref backend.
Disables the eglot backend when inside a `.g8' template."
  (unless (string-match-p ".g8" default-directory)
    'eglot))

(provide 'jacob-eglot)

;;; jacob-eglot.el ends here
