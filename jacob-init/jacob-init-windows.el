(if (string-equal system-type "windows-nt")
    (use-package emacs
      :config
      (setq w32-pass-rwindow-to-system nil)
      (setq w32-pass-apps-to-system nil)
      (setq w32-rwindow-modifier 'super)
      (setq w32-apps-modifier 'hyper)))
