(use-package lsp-mode
  :ensure t
  :hook
  ((php-mode-hook) . lsp)
  (lsp-mode-hook . lsp-enable-which-key-integration)
  :commands lsp
  :init
  (setq lsp-completion-enable-additional-text-edit nil)
  (setq lsp-prefer-capf nil)
  (setq lsp-prefer-flymake nil)
  :config
  (define-key xah-fly-dot-keymap (kbd "l") lsp-command-map))

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode)

(use-package lsp-ivy
  :ensure t
  :commands lsp-ivy-workspace-symbol)

(use-package lsp-python-ms
  :ensure t
  :disabled
  :init (setq lsp-python-ms-auto-install-server t))
