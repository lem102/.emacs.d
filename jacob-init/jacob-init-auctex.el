(use-package tex
  :ensure auctex
  :config
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq-default japanese-TeX-error-messages nil)
  (TeX-global-PDF-mode 0))
