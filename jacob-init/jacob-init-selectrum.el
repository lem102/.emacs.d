(use-package prescient
  :ensure t)

(use-package selectrum-prescient
  :ensure t)

(use-package marginalia
  :ensure t
  :config
  (setq marginalia-annotators '(marginalia-annotators-heavy nil marginalia-annotators-light)))

(use-package selectrum
  :ensure t
  :after prescient selectrum-prescient marginalia
  :config
  (marginalia-mode 1)
  (selectrum-mode 1)
  (selectrum-prescient-mode 1))
