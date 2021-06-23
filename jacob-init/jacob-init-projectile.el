(jacob-is-installed 'projectile
  (projectile-mode 1)

  (with-eval-after-load 'xah-fly-keys
    (define-key xah-fly-dot-keymap (kbd "p") 'projectile-command-map))
  
  (with-eval-after-load 'projectile
    (setq projectile-completion-system 'default)
    (setq projectile-indexing-method 'alien)))
