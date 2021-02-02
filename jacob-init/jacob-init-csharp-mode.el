(use-package csharp-mode
  :ensure t
  :init
  (defun my-csharp-mode-setup ()
    (setq c-syntactic-indentation t)
    (c-set-style "ellemtel")
    (setq c-basic-offset 4)
    (load-file "~/.emacs.d/myLisp/namespace.el"))
  :hook (csharp-mode-hook . my-csharp-mode-setup)
  :mode ("\\.cs\\$" . csharp-mode))
