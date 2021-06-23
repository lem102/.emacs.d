(jacob-is-installed 'restart-emacs
  (with-eval-after-load 'xah-fly-keys
    (define-key jacob-config-keymap (kbd "R") 'restart-emacs)))
