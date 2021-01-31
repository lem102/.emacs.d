(use-package omnisharp
  :ensure t
  :defer t
  :bind
  (:map jacob-omnisharp-keymap
        ("u" . omnisharp-fix-usings)
        ("U" . omnisharp-find-usages)
        ("i" . omnisharp-find-implementations)
        ("d" . omnisharp-go-to-definition)
        ("r" . omnisharp-rename)
        ("a" . omnisharp-run-code-action-refactoring)
        ("o" . omnisharp-start-omnisharp-server)
        ("O" . omnisharp-stop-server))
  :config
  (define-prefix-command 'jacob-omnisharp-keymap)
  (define-key xah-fly-dot-keymap (kbd "o") jacob-omnisharp-keymap)
  (setq omnisharp-company-ignore-case nil)
  (if (boundp jacob-omnisharp-file-path)
      (setq omnisharp-server-executable-path (expand-file-name jacob-omnisharp-file-path))))

(use-package emacs
  :after company omnisharp
  :config
  (add-hook 'omnisharp-mode-hook (lambda ()
                                   (add-to-list (make-local-variable 'company-backends)
                                                '(company-omnisharp)))))
