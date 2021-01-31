(use-package csharp-mode
  :ensure t
  :config
  (defun my-csharp-mode-setup ()
    (setq c-syntactic-indentation t)
    (c-set-style "ellemtel")
    (setq c-basic-offset 4)
    (load-file "~/.emacs.d/myLisp/namespace.el")
    (yas-minor-mode 1))
  :hook (csharp-mode-hook . my-csharp-mode-setup)
  :mode ("\\.cs\\$" . csharp-mode))
