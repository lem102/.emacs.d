(use-package company
  :ensure t
  :config
  (setq company-idle-delay 0.5)
  (setq company-minimum-prefix-length 3)
  :hook ((java-mode-hook csharp-mode-hook) . company-mode))
