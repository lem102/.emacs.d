(use-package company
  :ensure t
  :defer t
  :hook ((csharp-mode-hook java-mode-hook) . company-mode)
  :config
  (setq company-idle-delay 0.5)
  (setq company-minimum-prefix-length 3))

(use-package company-php
  :ensure t
  :after company
  :config
  (add-to-list (make-local-variable 'company-backends)
               '(company-ac-php-backend)))
