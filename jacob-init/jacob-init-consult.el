(use-package consult
  :ensure t
  :bind
  (:map xah-fly-dot-keymap
        ("s" . consult-line))
  (:map xah-fly-c-keymap
        ("j" . consult-recent-file))
  (:map xah-fly-leader-key-map
        ("v" . consult-yank)))
