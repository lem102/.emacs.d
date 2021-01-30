(use-package xah-fly-keys
  :ensure t

  :after key-chord
  :demand

  :init
  (setq xah-fly-use-control-key nil)

  ;; This is a keyboard macro that enters insert mode, presses a backspace, then returns to command mode.
  ;; It's purpose is so I can bind "D" in command mode to whatever backspace does in any given buffer.
  (fset 'backspace
        [?f backspace home])

  (fset 'enter
        [return])

  (defun jacob-xah-command-binds ()
    "Set custom keys for xah-fly-keys keybindings."
    (interactive)
    (define-key xah-fly-command-map (kbd "a") 'counsel-M-x)
    (define-key xah-fly-command-map (kbd "s") 'enter)
    (define-key xah-fly-command-map (kbd "8") 'er/expand-region)
    (define-key xah-fly-command-map (kbd "4") 'jacob-split-window-below-select-new)
    ;; 1 can be rebound, is bound to a inferior version of expand region
    (define-key xah-fly-command-map (kbd "2") 'jacob-quit-popup-window))

  :config
  (load-file (expand-file-name "~/.emacs.d/myLisp/jacob-xah-modified-commands.el"))

  (define-prefix-command 'jacob-config-keymap)
  (xah-fly-keys-set-layout "qwerty")
  (xah-fly-keys 1)

  (add-hook 'xah-fly-command-mode-activate-hook 'jacob-xah-command-binds)
  (jacob-xah-command-binds) ;; call it on startup so binds are set without calling xah-fly-command-mode-activate first.

  (key-chord-define-global "fd" 'xah-fly-command-mode-activate)

  ;; switches to insert mode upon entering the minibuffer.
  (add-hook 'minibuffer-setup-hook 'xah-fly-insert-mode-activate)
  ;; switches back to command mode after exiting the minibuffer.
  (add-hook 'minibuffer-exit-hook 'xah-fly-command-mode-activate)

  :bind
  (:map jacob-config-keymap
        ("r" . jacob-config-reload)
        ("R" . restart-emacs)
        ("e" . jacob-config-visit)
        ("c" . jacob-org-src-block)
        ("p" . jacob-recompile-packages)
        ("t" . jacob-display-time))
  (:map xah-fly-e-keymap
        ("k". jacob-xah-insert-paren)
        ("l". jacob-xah-insert-square-bracket)
        ("j". jacob-xah-insert-brace)
        ("u". jacob-xah-insert-ascii-double-quote)
        ("i". jacob-xah-insert-ascii-single-quote)
        ("m" . xah-insert-hyphen)
        ("," . xah-insert-low-line)
        ("." . jacob-insert-equals)
        ("/" . jacob-insert-plus)
        ("z" . jacob-insert-apostrophe)
        ("x" . jacob-insert-at)
        ("c" . jacob-insert-hash)
        ("d" . backspace)
        ("v" . jacob-insert-tilde))
  (:map xah-fly-dot-keymap
        ("c" . jacob-config-keymap))
  (:map xah-fly-leader-key-map
        ("4" . jacob-split-window-right-select-new))
  (:map xah-fly-w-keymap
        ("n" . eval-and-replace))
  (:map xah-fly-t-keymap
        ("j" . kill-this-buffer)))
