(when (eq system-type 'windows-nt)
  (setq w32-pass-rwindow-to-system nil)
  (setq w32-rwindow-modifier 'super)
  (setq w32-apps-modifier 'hyper)

  (setq ls-lisp-use-insert-directory-program nil)
  (setq ls-lisp-dirs-first t))
