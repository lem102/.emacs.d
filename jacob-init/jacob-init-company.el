(jacob-is-installed 'company
  (add-hook 'java-mode-hook 'company-mode)
  (add-hook 'csharp-mode-hook 'company-mode)
  
  (with-eval-after-load 'company
    (setq company-idle-delay 0.5)
    (setq company-minimum-prefix-length 3)))
