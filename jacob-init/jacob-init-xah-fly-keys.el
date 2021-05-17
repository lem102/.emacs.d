(use-package xah-fly-keys
  :ensure t
  :demand
  ;; :after key-chord
  :init
  (setq xah-fly-use-control-key nil)
  (setq xah-fly-use-meta-key t)
  :config
  (define-prefix-command 'jacob-config-keymap)
  (xah-fly-keys-set-layout "qwerty")
  (xah-fly-keys 1)

  ;; TODO: figure out how to bind these using use-package
  (let ((map xah-fly-dot-keymap))
    ;; (define-key map (kbd "p") project-prefix-map)
    (define-key map (kbd "v") vc-prefix-map))

  (add-hook 'after-init-hook (lambda () (unbind-key "C-<kp-subtract>") (define-key global-map (kbd "C-<kp-subtract> C-`") 'jacob-insert-asterisk)))

  :hook
  (minibuffer-setup-hook . xah-fly-insert-mode-activate)
  (minibuffer-exit-hook . xah-fly-command-mode-activate)
  ;; :chords
  ;; ("fd" . xah-fly-command-mode-activate)
  :bind
  (:map xah-fly-command-map
        ("a" . execute-extended-command)
        ("s" . jacob-enter-kmacro)
        ;; ("h" . jacob-back-to-indentation-or-beginning-of-line)
        ;; (";" . move-end-of-line)
        ("4" . jacob-split-window-below-select-new)
        ("2" . jacob-quit-popup-window))
  (:map xah-fly-dot-keymap
        ("c" . jacob-config-keymap))
  (:map jacob-config-keymap
        ("r" . jacob-config-reload)
        ("e" . jacob-config-visit)
        ("c" . jacob-org-src-block)
        ("p" . jacob-recompile-packages)
        ("t" . jacob-display-time))
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
        ("v" . jacob-insert-tilde)
        ("e" . jacob-insert-dollar-sign)
        ("r" . jacob-insert-caret)
        ("o" . jacob-insert-ampersand))
  (:map xah-fly-leader-key-map
        ("4" . jacob-split-window-right-select-new))
  (:map xah-fly-w-keymap
        ("n" . jacob-eval-and-replace))
  (:map xah-fly-t-keymap
        ("j" . kill-this-buffer)))
