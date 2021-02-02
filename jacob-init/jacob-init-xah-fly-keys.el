(use-package xah-fly-keys
  :ensure t
  :demand
  :after key-chord
  :init
  (setq xah-fly-use-control-key nil)
  (setq xah-fly-use-meta-key t)
  :config
  (define-prefix-command 'jacob-config-keymap)
  (xah-fly-keys-set-layout "qwerty")
  (xah-fly-keys 1)

  ;; TODO: install the key chord extension for use package
  (key-chord-define-global "fd" 'xah-fly-command-mode-activate)
  (key-chord-define-global "fj" 'avy-goto-char)
  (key-chord-define-global "fk" 'avy-goto-end-of-line)

  ;; TODO: figure out how to bind these using use-package
  (let ((map xah-fly-dot-keymap))
    (define-key map (kbd "p") project-prefix-map)
    (define-key map (kbd "v") vc-prefix-map))

  :hook
  (minibuffer-setup-hook . xah-fly-insert-mode-activate)
  (minibuffer-exit-hook . xah-fly-command-mode-activate)
  :bind
  (:map xah-fly-command-map
        ("s" . jacob-enter-kmacro)
        ("8" . er/expand-region)
        ("4" . jacob-split-window-below-select-new)
        ("2" . jacob-quit-popup-window)
        ("," . ace-window))
  (:map xah-fly-dot-keymap
        ("c" . jacob-config-keymap)
        ("s" . consult-line))
  (:map jacob-config-keymap
        ("r" . jacob-config-reload)
        ("R" . restart-emacs)
        ("e" . jacob-config-visit)
        ("c" . jacob-org-src-block)
        ("p" . jacob-recompile-packages)
        ("t" . jacob-display-time))
  (:map xah-fly-c-keymap
        ("j" . consult-recent-file))
  (:map xah-fly-e-keymap
        ("k" . jacob-xah-insert-paren)
        ("l" . jacob-xah-insert-square-bracket)
        ("j" . jacob-xah-insert-brace)
        ("u" . jacob-xah-insert-ascii-double-quote)
        ("i" . jacob-xah-insert-ascii-single-quote)
        ("m" . xah-insert-hyphen)
        ("," . xah-insert-low-line)
        ("." . jacob-insert-equals)
        ("/" . jacob-insert-plus)
        ("z" . jacob-insert-apostrophe)
        ("x" . jacob-insert-at)
        ("c" . jacob-insert-hash)
        ("d" . jacob-backspace-kmacro)
        ("v" . jacob-insert-tilde))
  (:map xah-fly-leader-key-map
        ("4" . jacob-split-window-right-select-new)
        ("v" . consult-yank))
  (:map xah-fly-w-keymap
        ("n" . eval-and-replace))
  (:map xah-fly-t-keymap
        ("j" . kill-this-buffer))
  (:map xah-fly-r-keymap
        ("c" . kmacro-set-counter)))
