(jacob-is-installed 'eglot
  (add-hook 'java-mode-hook 'eglot-ensure)
  (add-hook 'csharp-mode-hook 'eglot-ensure)
  (with-eval-after-load 'eglot
    (defun jacob-eglot-eclipse-jdt-contact
        (interactive)
      "Contact with the jdt server input INTERACTIVE."
      (let ((cp (getenv "CLASSPATH"))
            (jdt-home jacob-eclipse-jdt-file-path))
        (setenv "CLASSPATH" (concat cp ":" jdt-home))
        (unwind-protect (eglot--eclipse-jdt-contact nil)
          (setenv "CLASSPATH" cp))))
    
    (setcdr (assq 'java-mode eglot-server-programs) #'jacob-eglot-eclipse-jdt-contact)

    (add-to-list 'eglot-server-programs
                 `(csharp-mode . ("d:/programming/OmniSharp/omnisharp-win-x64/OmniSharp.exe" "-lsp")))
    
    (with-eval-after-load 'xah-fly-keys
      (define-prefix-command 'jacob-eglot-keymap)
      (define-key xah-fly-dot-keymap (kbd "e") jacob-eglot-keymap)
      (define-key jacob-eglot-keymap (kbd "a") 'eglot-code-actions)
      (define-key jacob-eglot-keymap (kbd "r") 'eglot-rename)
      ;; not sure if these should be bound here
      (define-key jacob-eglot-keymap (kbd "d") 'xref-find-definitions)
      (define-key jacob-eglot-keymap (kbd "u") 'xref-find-references))))
