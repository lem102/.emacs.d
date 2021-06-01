(use-package eglot
  :ensure t
  :config
  (defun jacob-eglot-eclipse-jdt-contact
      (interactive)
    "Contact with the jdt server input INTERACTIVE."
    (let ((cp (getenv "CLASSPATH"))
          (jdt-home jacob-eclipse-jdt-file-path))
      (setenv "CLASSPATH" (concat cp ":" jdt-home))
      (unwind-protect (eglot--eclipse-jdt-contact nil)
        (setenv "CLASSPATH" cp))))
  
  (setcdr (assq 'java-mode eglot-server-programs) #'jacob-eglot-eclipse-jdt-contact)

  (define-prefix-command 'jacob-eglot-keymap)

  (add-to-list 'eglot-server-programs
               `(csharp-mode . ("d:/programming/OmniSharp/omnisharp-win-x64/OmniSharp.exe" "-lsp")))
  :bind
  (:map xah-fly-dot-keymap
        ("e" . jacob-eglot-keymap))
  (:map jacob-eglot-keymap
        ("a" . eglot-code-actions)
        ("r" . eglot-rename)
        ;; not sure if these should be bound here
        ("d" . xref-find-definitions)
        ("u" . xref-find-references))
  :hook ((java-mode-hook . eglot-ensure)
         (csharp-mode-hook . eglot-ensure)))
