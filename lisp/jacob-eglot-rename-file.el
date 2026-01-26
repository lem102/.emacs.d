;;; jacob-eglot-rename-file.el --- Allow eglot to rename files

;;; Commentary:
;; 

;; phased approach to solving this problem.

;; phase 1. override `eglot--apply-workspace-edit' so that file rename workspace edits are dropped.
;; phase 2. filter the args of `eglot--apply-workspace-edit' so that file rename workspace edits are removed from the `wedit' argument.
;; phase 3. override `eglot--apply-workspace-edit' so that file renames are correctly handled by this function.

;; i move directly to phase 3

;;; Code:

(defun jacob-eglot--apply-workspace-edit (wedit origin)
  "Override to teach eglot about `FileRename'."
  (eglot--dbind ((WorkspaceEdit) changes documentChanges) wedit
    (let* ((file-renames (seq-filter (eglot--lambda ((FileRename) oldUri newUri)
                                       (and oldUri newUri))
                                     documentChanges))
           (text-document-edits (seq-filter (eglot--lambda ((TextDocumentEdit) textDocument edits)
                                              (and textDocument edits))
                                            documentChanges))
           (prepared                     ; prepared are the text document edits
            (mapcar (eglot--lambda ((TextDocumentEdit) textDocument edits)
                      (eglot--dbind ((VersionedTextDocumentIdentifier) uri version)
                          textDocument
                        (list (eglot-uri-to-path uri) edits version)))
                    text-document-edits)))
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
            (apply)))))
      (dolist (file-rename file-renames)
        (eglot--dbind ((FileRename) oldUri newUri)
            file-rename
          (rename-file (eglot-uri-to-path oldUri) (eglot-uri-to-path newUri)))))))

(advice-add #'eglot--apply-workspace-edit :override #'jacob-eglot--apply-workspace-edit)
(add-to-list 'eglot--lsp-interface-alist '(FileRename (:oldUri) (:newUri)))

;; (advice-remove #'eglot--apply-workspace-edit #'jacob-eglot--apply-workspace-edit)

(provide 'jacob-eglot-rename-file)

;;; jacob-eglot-rename-file.el ends here
