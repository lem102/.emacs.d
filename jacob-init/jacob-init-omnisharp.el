(use-package omnisharp
  :ensure t
  :after company
  :config
  (define-prefix-command 'jacob-omnisharp-keymap)
  
  (setq omnisharp-company-ignore-case nil)
  
  (if (boundp 'jacob-omnisharp-file-path)
      (setq omnisharp-server-executable-path (expand-file-name "d:/programming/OmniSharp/omnisharp-win-x64/OmniSharp.exe")))

  (add-hook 'omnisharp-mode-hook (lambda ()
                                   (add-to-list (make-local-variable 'company-backends)
                                                '(company-omnisharp))))
  
  :hook (csharp-mode-hook . omnisharp-mode)
  :bind
  (:map xah-fly-dot-keymap
        ("o" . jacob-omnisharp-keymap))
  (:map jacob-omnisharp-keymap
        ("u" . omnisharp-fix-usings)
        ("U" . omnisharp-find-usages)
        ("i" . omnisharp-find-implementations)
        ("d" . omnisharp-go-to-definition)
        ("r" . omnisharp-rename)
        ("a" . omnisharp-run-code-action-refactoring)
        ("o" . omnisharp-start-omnisharp-server)
        ("O" . omnisharp-stop-server)))
