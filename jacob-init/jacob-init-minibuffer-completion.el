(use-package orderless
  :ensure t
  :config
  (setq completion-styles '(orderless)))

(use-package prescient
  :ensure t
  :config
  (prescient-persist-mode 1))

(use-package selectrum-prescient
  :ensure t
  :config
  (selectrum-prescient-mode 1))

(use-package marginalia
  :ensure t
  :config
  (marginalia-mode 1))

(use-package selectrum
  :ensure t
  :config
  (setq selectrum-display-action nil)
  (setq selectrum-max-window-height 25)
  (setq enable-recursive-minibuffers t)
  (selectrum-mode 1))

(use-package consult
  :ensure t
  :config
  (setq completion-in-region-function 'consult-completion-in-region)
  (setq consult-preview-key 'any)
  (dolist (cmd '(consult-bookmark consult-recent-file consult-buffer))
    (setf (alist-get cmd consult-config) `(:preview-key ,nil)))
  :bind
  ("C-z SPC e c f" . consult-buffer)
  ("C-z SPC e c n" . consult-line)
  (:map xah-fly-dot-keymap
        ("s" . consult-line))
  (:map xah-fly-c-keymap
        ("j" . consult-recent-file))
  (:map xah-fly-leader-key-map
        ("v" . consult-yank)))
