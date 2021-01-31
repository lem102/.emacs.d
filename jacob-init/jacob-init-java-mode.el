(defun jacob-java-mode-setup ()
  (setq c-syntactic-indentation t)
  (c-set-style "ellemtel")
  (setq c-basic-offset 4)
  (yas-minor-mode 1)
  (eglot-ensure)
  (company-mode 1))

(add-hook 'java-mode-hook 'jacob-java-mode-setup)
