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

(defun jacob-eglot--apply-workspace-edit (wedit origin)
  "Apply (or offer to apply) the workspace edit WEDIT.
ORIGIN is a symbol designating the command that originated this
edit proposed by the server.

This advised version discards document changes that are not text
document edits."
  (eglot--dbind ((WorkspaceEdit) changes documentChanges) wedit
    (let ((prepared-tde
           (mapcar (eglot--lambda ((TextDocumentEdit) textDocument edits)
                     (eglot--dbind ((VersionedTextDocumentIdentifier) uri version)
                         textDocument
                       (list (eglot-uri-to-path uri) edits version)))
                   (seq-filter (lambda (dc)
                                 (ignore-errors (eglot--check-object 'TextDocumentEdit dc)))
                               documentChanges)))
          (prepared-rename-file
           (mapcar (eglot--lambda ((RenameFile) oldUri newUri)
                     (cons (eglot-uri-to-path oldUri) (eglot-uri-to-path newUri)))
                   (seq-filter (lambda (dc)
                                 (ignore-errors (eglot--check-object 'RenameFile dc)))
                               documentChanges))))
      (unless (and changes documentChanges)
        ;; We don't want double edits, and some servers send both
        ;; changes and documentChanges.  This unless ensures that we
        ;; prefer documentChanges over changes.
        (cl-loop for (uri edits) on changes by #'cddr
                 do (push (list (eglot-uri-to-path uri) edits) prepared-tde)))
      (cl-flet ((notevery-visited-p ()
                  (cl-notevery #'find-buffer-visiting
                               (mapcar #'car prepared-tde)))
                (accept-p ()
                  (y-or-n-p
                   (format "[eglot] Server wants to edit:\n%sProceed? "
                           (cl-loop
                            for (f eds _) in prepared-tde
                            concat (format
                                    "  %s (%d change%s)\n"
                                    f (length eds)
                                    (if (> (length eds) 1) "s" ""))))))
                (apply ()
                  (cl-loop for edit in prepared-tde
                           for (path edits version) = edit
                           do (with-current-buffer (find-file-noselect path)
                                (eglot--apply-text-edits edits version))
                           finally (eldoc) (eglot--message "Edit successful!"))))
        (let ((decision (eglot--confirm-server-edits origin prepared-tde)))
          (cond
           ((or (eq decision 'diff)
                (and (eq decision 'maybe-diff) (notevery-visited-p)))
            (eglot--propose-changes-as-diff prepared-tde))
           ((or (memq decision '(t summary))
                (and (eq decision 'maybe-summary) (notevery-visited-p)))
            (when (accept-p) (apply)))
           (t
            (apply)))))

      (jacob-eglot-rename-files prepared-rename-file))))

(defun jacob-eglot-rename-files (filename-alist)
  "Rename files according to FILENAME-ALIST.

FILENAME-ALIST is an alist of (old-name . new-name) pairs."
  (dolist (pair filename-alist)
    (let ((old (car pair))
          (new (cdr pair)))
      ;; TODO: test this code
      (eglot--signal-textDocument/didClose)
      (dired-rename-file old new nil)
      (eglot--signal-textDocument/didOpen))))

(provide 'jacob-eglot)

;;; jacob-eglot.el ends here
