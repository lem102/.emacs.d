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

(provide 'jacob-eglot)

;;; jacob-eglot.el ends here
