(use-package avy
  :ensure t
  :after key-chord
  :defer 0.1
  :config
  (setq avy-style 'at-full)
  (setq avy-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l ?\;))
  (setq avy-orders-alist '((avy-goto-end-of-line . avy-order-closest)
                           (avy-goto-word-or-subword-1 . avy-order-closest)))
  (setq avy-all-windows 'all-frames)
  (key-chord-define-global "fj" 'avy-goto-char)
  (key-chord-define-global "fk" 'avy-goto-end-of-line))
