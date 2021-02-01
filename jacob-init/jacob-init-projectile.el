(use-package projectile
  :ensure t
  :defer 2
  :config
  (projectile-mode t)
  (setq projectile-completion-system 'default)
  (define-key xah-fly-dot-keymap (kbd "p") projectile-command-map))
