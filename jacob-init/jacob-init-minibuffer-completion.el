(use-package marginalia
  :ensure t
  :config
  (marginalia-mode 1))

(use-package vertico
  :ensure t
  :after marginalia
  :config
  (vertico-mode 1))

(use-package consult
  :ensure t
  :config
  (setq completion-in-region-function 'consult-completion-in-region)
  (setq consult-preview-max-size 0)
  :bind
  (:map xah-fly-dot-keymap
        ("s" . consult-line))
  (:map xah-fly-c-keymap
        ("j" . consult-recent-file))
  (:map xah-fly-leader-key-map
        ("v" . consult-yank)))

