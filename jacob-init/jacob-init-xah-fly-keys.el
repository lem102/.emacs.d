(use-package xah-fly-keys
  :ensure t
  :demand
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

  (defun xah-jacob-kill-word (repetitions)
    (interactive "p")
    (when (use-region-p)
      (delete-region (region-beginning) (region-end)))
    (kill-word repetitions))

  (defun xah-jacob-backward-kill-word (repetitions)
    (interactive "p")
    (when (use-region-p)
      (delete-region (region-beginning) (region-end)))
    (backward-kill-word repetitions))

  (defun xah-jacob-beginning-of-line-or-block (repetitions)
    (interactive "p")
    (dotimes (i repetitions)
      (xah-beginning-of-line-or-block)))

  (defun xah-jacob-end-of-line-or-block (repetitions)
    (interactive "p")
    (dotimes (i repetitions)
      (xah-end-of-line-or-block)))

  (xah-fly-keys-off)
  
  
  :bind
  ("C-z r" . xah-jacob-kill-word)
  ("C-z e" . xah-jacob-backward-kill-word)
  ("C-z h" . xah-jacob-beginning-of-line-or-block)
  ("C-z ;" . xah-jacob-end-of-line-or-block)
  ("C-z c" . xah-copy-line-or-region)
  ("C-z x" . xah-cut-line-or-region)
  ("C-z ." . xah-forward-right-bracket)
  ("C-z m" . xah-backward-left-bracket)
  ("C-z /" . xah-goto-matching-bracket)
  ("C-z d" . xah-delete-backward-char-or-bracket-text)
  ("C-z 0" . xah-pop-local-mark-ring)
  ("C-z v" . xah-paste-or-paste-previous)
  ("C-z w" . xah-shrink-whitespaces)
  ("C-z p p" . jacob-xah-insert-paren)
  ("C-z p b" . jacob-xah-insert-square-bracket)
  ("C-z p c" . jacob-xah-insert-brace)
  ("C-z p a" . jacob-xah-insert-angled-bracket)
  ("C-z p q" . jacob-xah-insert-ascii-double-quote)
  ("C-z p s" . jacob-xah-insert-ascii-single-quote)
  (:map xah-fly-command-map
        ("a" . execute-extended-command)
        ("s" . jacob-enter-kmacro)
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
