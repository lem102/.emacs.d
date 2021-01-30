(use-package ivy
  :ensure t
  :defer 0.1
  :config
  (setq ivy-initial-inputs-alist nil)
  (setq enable-recursive-minibuffers t)
  (setq completing-read-function 'ivy-completing-read)
  :bind
  (:map xah-fly-c-keymap
        ("e" . counsel-find-file))
  (:map xah-fly-dot-keymap
        ("s" . swiper))
  (:map xah-fly-h-keymap
        ("j" . counsel-describe-function)
        ("l" . counsel-describe-variable))
  (:map xah-fly-leader-key-map
        ("v" . counsel-yank-pop)
        ("f" . ivy-switch-buffer)))

(use-package swiper
  :ensure t
  :after ivy)

(use-package counsel
  :ensure t
  :after ivy)
