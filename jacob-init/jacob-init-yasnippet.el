(use-package yasnippet
  :ensure t
  :hook
  (((eshell-mode-hook web-mode-hook python-mode-hook java-mode-hook csharp-mode-hook php-mode-hook) . yas-minor-mode))
  :config
  (yas-reload-all))
