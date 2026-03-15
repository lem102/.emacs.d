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

(defun jacob-eglot--apply-workspace-edit (wedit origin)
  "Apply (or offer to apply) the workspace edit WEDIT.
ORIGIN is a symbol designating the command that originated this
edit proposed by the server.

This advised version discards document changes that are not text
document edits."
  ;; TODO: make version of this that can handle RenameFile
  (eglot--dbind ((WorkspaceEdit) changes documentChanges) wedit
    (let ((prepared
           (mapcar (eglot--lambda ((TextDocumentEdit) textDocument edits)
                     (eglot--dbind ((VersionedTextDocumentIdentifier) uri version)
                         textDocument
                       (list (eglot-uri-to-path uri) edits version)))
                   (seq-filter (lambda (dc)
                                 (ignore-errors (eglot--check-object 'TextDocumentEdit dc)))
                               documentChanges))))
      (unless (and changes documentChanges)
        ;; We don't want double edits, and some servers send both
        ;; changes and documentChanges.  This unless ensures that we
        ;; prefer documentChanges over changes.
        (cl-loop for (uri edits) on changes by #'cddr
                 do (push (list (eglot-uri-to-path uri) edits) prepared)))
      (cl-flet ((notevery-visited-p ()
                  (cl-notevery #'find-buffer-visiting
                               (mapcar #'car prepared)))
                (accept-p ()
                  (y-or-n-p
                   (format "[eglot] Server wants to edit:\n%sProceed? "
                           (cl-loop
                            for (f eds _) in prepared
                            concat (format
                                    "  %s (%d change%s)\n"
                                    f (length eds)
                                    (if (> (length eds) 1) "s" ""))))))
                (apply ()
                  (cl-loop for edit in prepared
                           for (path edits version) = edit
                           do (with-current-buffer (find-file-noselect path)
                                (eglot--apply-text-edits edits version))
                           finally (eldoc) (eglot--message "Edit successful!"))))
        (let ((decision (eglot--confirm-server-edits origin prepared)))
          (cond
           ((or (eq decision 'diff)
                (and (eq decision 'maybe-diff) (notevery-visited-p)))
            (eglot--propose-changes-as-diff prepared))
           ((or (memq decision '(t summary))
                (and (eq decision 'maybe-summary) (notevery-visited-p)))
            (when (accept-p) (apply)))
           (t
            (apply))))))))

(provide 'jacob-eglot)

;;; jacob-eglot.el ends here
