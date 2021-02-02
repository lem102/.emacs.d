(use-package yasnippet
  :ensure t
  :config
  (yas-reload-all)
  :hook ((java-mode-hook csharp-mode-hook web-mode-hook) . yas-minor-mode))
