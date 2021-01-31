(use-package yasnippet
  :ensure t
  :hook
  (((eshell-mode-hook python-mode-hook) . yas-minor-mode))
  :config
  (yas-reload-all))
