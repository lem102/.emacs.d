(use-package flycheck
  :config
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))
  :hook (csharp-mode-hook . flycheck-mode))
