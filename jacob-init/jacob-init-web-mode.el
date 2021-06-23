(jacob-is-installed 'web-mode
  (defun jacob-web-mode-config ()
    (setq-local electric-pair-pairs '((?\" . ?\") (?\< . ?\>)))
    (yas-activate-extra-mode 'html-mode))

  (add-hook 'web-mode-hook 'jacob-web-mode-config)

  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.cshtml\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.css\\'" . web-mode))

  (with-eval-after-load 'web-mode
    (setq web-mode-engines-alist '(("razor" . "\\.cshtml\\'")))
    (setq web-mode-markup-indent-offset 2)
    (setq web-mode-css-indent-offset 2)
    (setq web-mode-code-indent-offset 2)))
