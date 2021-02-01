(use-package elisp-mode
  :init
  (defun jacob-elisp-mode-setup ()
    (flycheck-mode 1))
  :hook (emacs-lisp-mode-hook . jacob-elisp-mode-setup))
