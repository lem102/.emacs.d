(defun jacob-java-mode-setup ()
  (setq c-syntactic-indentation t)
  (c-set-style "ellemtel")
  (setq c-basic-offset 4))

(add-hook 'java-mode-hook 'jacob-java-mode-setup)
