;; ** omnisharp
;; FIXME: if company mode is not started before csharp mode is entered, omnisharp mode will not activate
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
  ;; at this point, company mode is enabled.
  (define-prefix-command 'jacob-omnisharp-keymap)
  (define-key xah-fly-dot-keymap (kbd "o") jacob-omnisharp-keymap)

  (setq omnisharp-company-ignore-case nil)
  (setq omnisharp-server-executable-path (expand-file-name jacob-omnisharp-file-path))

  (defun jacob-csharp-indent-or-complete ()
    (interactive)
    (if (region-active-p)
        (c-indent-line-or-region :region (region-bounds))
      (let ((old-point (point)))
        (c-indent-line-or-region)
        (if (eq old-point (point))
            (call-interactively 'counsel-company)))))

  (define-key csharp-mode-map (kbd "<tab>") 'jacob-csharp-indent-or-complete)
  :hook (csharp-mode-hook . omnisharp-mode))

;; *** add omnisharp to company backend
(use-package emacs
  :after company omnisharp
  :config
  (add-hook 'omnisharp-mode-hook (lambda ()
                                   (add-to-list (make-local-variable 'company-backends)
                                                '(company-omnisharp)))))
