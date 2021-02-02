(use-package expand-region
  :ensure t
  :config
  (setq expand-region-contract-fast-key "9")
  :bind
  (:map xah-fly-command-map
        ("8" . er/expand-region)))
