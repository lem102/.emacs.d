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
  (save-excursion
    (goto-char (point-min))
    (while (search-forward (char-to-string 13) nil t)
      (replace-match ""))))

(defun jacob-eglot-xref-backend ()
  "Custom eglot xref backend.
Disables the eglot backend when inside a `.g8' template."
  (unless (string-match-p ".g8" default-directory)
    'eglot))

(defun jacob-eglot-metals-bsp-switch ()
  "Call the bsp-switch command on the metals server."
  (interactive)
  (jsonrpc-notify (eglot-current-server)
                  :workspace/executeCommand
                  (list :command "bsp-switch")))

;; TODO: wip
(cl-defmethod eglot-client-capabilities :around (server)
  "Try add support for workspace/willRenameFiles notification."
  (let* ((base (cl-call-next-method)))
    (setf (cl-getf (cl-getf base :workspace)
                   :willRenameFiles)
          t)
    base))

;; i have added this ^implementation^. i wonder how eglot will then function given there is no implementation....

(cl-defmethod eglot-handle-notification
  (_server (_method (eql workspace/didRenameFiles))
           &allow-other-keys)
  ;; is this needed? or is this to handle a notification from the server?
  "Handle the workspace/willRenameFiles notification."
  (debug nil "jacob just did a sick rename"))

(defun jacob-eglot--after-set-visited-file-name-hook ()
  "Maybe send workspace/willRenameFiles to server.

Intended to be an advice :after `eglot--after-set-visited-file-name-hook'."
  (when (and eglot--managed-mode buffer-file-name)
    (jacob-eglot--signal-workspace/willRenameFiles)))

(advice-add #'eglot--after-set-visited-file-name-hook :after #'jacob-eglot--after-set-visited-file-name-hook)

(defun jacob-eglot--signal-workspace/willRenameFiles ()
  "Send workspace/willRenameFiles to server."
  (let* ((new-file (eglot-path-to-uri (buffer-file-name))) ; HACK: temp hack, won't work if renamed outside of current dir (which is whole point orz)
         (dir (file-name-directory new-file)))
    (eglot--request (eglot--current-server-or-lose)
                    :workspace/willRenameFiles `( :files [( :oldUri ,(file-name-concat dir (buffer-last-name)) ; TODO: how to get previous name of the file before rename?
                                                            :newUri ,new-file)]))))

(provide 'jacob-eglot)

;;; jacob-eglot.el ends here
