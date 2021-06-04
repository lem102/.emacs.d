(use-package flycheck
  :disabled t
  :config
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))
  :hook (csharp-mode-hook . flycheck-mode))
