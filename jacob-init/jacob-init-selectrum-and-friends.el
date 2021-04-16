(use-package prescient
  :ensure t
  :config
  (prescient-persist-mode 1))

(use-package selectrum-prescient
  :ensure t
  :config
  (selectrum-prescient-mode 1))

(use-package marginalia
  :ensure t
  :config
  (setq marginalia-annotators '(marginalia-annotators-heavy nil marginalia-annotators-light))
  (marginalia-mode 1))

(use-package selectrum
  :ensure t
  :after prescient selectrum-prescient marginalia
  :config
  (selectrum-mode 1))

(use-package consult
  :ensure t
  :config
  (setq consult-preview-max-size 0)
  :bind
  (:map xah-fly-dot-keymap
        ("s" . consult-line))
  (:map xah-fly-c-keymap
        ("j" . consult-recent-file))
  (:map xah-fly-leader-key-map
        ("v" . consult-yank)))

