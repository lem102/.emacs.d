(use-package yasnippet
  :ensure t
  :config
  (yas-reload-all)
  :hook ((octave-mode-hook java-mode-hook csharp-mode-hook web-mode-hook) . yas-minor-mode))
