(use-package avy
  :ensure t
  :config

  (setq avy-style 'at)
  (setq avy-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l ?\;))
  (setq avy-orders-alist '((avy-goto-end-of-line . avy-order-closest)
                           (avy-goto-word-or-subword-1 . avy-order-closest)))
  (setq avy-dispatch-alist '((?x . avy-action-teleport)
                             (?t . avy-action-mark)
                             (?c . avy-action-copy)
                             (?v . avy-action-yank)
                             (?i . avy-action-ispell)
                             (?d . avy-action-zap-to-char)))
  (setq avy-all-windows 'all-frames)
  :chords
  ("fk" . avy-goto-end-of-line)
  ("fj" . avy-goto-char))
