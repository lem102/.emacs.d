(jacob-is-installed 'projectile
  (projectile-mode 1)

  (with-eval-after-load 'projectile
    (setq projectile-completion-system 'default)
    (setq projectile-indexing-method 'alien)))
