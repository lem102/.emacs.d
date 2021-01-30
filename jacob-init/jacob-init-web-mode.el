(use-package web-mode
  :ensure t
  :preface
  (defun jacob-web-mode-config ()
    (interactive)
    (setq-local electric-pair-pairs '((?\" . ?\") (?\< . ?\>)))
    (yas-activate-extra-mode 'html-mode))
  :config
  (setq web-mode-engines-alist
        '(("razor"	. "\\.cshtml\\'")
          ("blade" . "\\.blade.php\\'")))
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  :hook (web-mode-hook . jacob-web-mode-config)
  :mode (("\\.blade.php\\'" . web-mode)
         ("\\.html?\\'" . web-mode)
         ("\\.cshtml\\'" . web-mode)
         ("\\.css\\'" . web-mode)))
