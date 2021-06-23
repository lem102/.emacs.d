(jacob-is-installed 'expand-region
  (with-eval-after-load 'expand-region
    (setq expand-region-contract-fast-key "9"))
  (with-eval-after-load 'xah-fly-keys
    (define-key xah-fly-command-map (kbd "8") 'er/expand-region)))
