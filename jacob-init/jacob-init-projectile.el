(use-package projectile
  :ensure t
  :defer 2
  :config
  (projectile-mode t)
  (define-key xah-fly-dot-keymap (kbd "p") 'projectile-command-map)
  (setq projectile-completion-system 'default)
  (setq projectile-indexing-method 'alien))
