(use-package flycheck
  :ensure t
  :defer 2
  :config
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc)))
