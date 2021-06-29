(let ((map global-map))
  (define-key map (kbd "C-z r") 'xah-jacob-kill-word)
  (define-key map (kbd "C-z e") 'xah-jacob-backward-kill-word)
  (define-key map (kbd "C-z h") 'xah-jacob-beginning-of-line-or-block)
  (define-key map (kbd "C-z ;") 'xah-jacob-end-of-line-or-block)
  (jacob-is-installed 'xah-fly-keys
    (define-key map (kbd "C-z c") 'xah-copy-line-or-region)
    (define-key map (kbd "C-z x") 'xah-cut-line-or-region)
    (define-key map (kbd "C-z .") 'xah-forward-right-bracket)
    (define-key map (kbd "C-z m") 'xah-backward-left-bracket)
    (define-key map (kbd "C-z /") 'xah-goto-matching-bracket)
    (define-key map (kbd "C-z d") 'xah-delete-backward-char-or-bracket-text)
    (define-key map (kbd "C-z 0") 'xah-pop-local-mark-ring)
    (define-key map (kbd "C-z v") 'xah-paste-or-paste-previous)
    (define-key map (kbd "C-z w") 'xah-shrink-whitespaces))
  (define-key map (kbd "C-z p p") 'jacob-xah-insert-paren)
  (define-key map (kbd "C-z p b") 'jacob-xah-insert-square-bracket)
  (define-key map (kbd "C-z p c") 'jacob-xah-insert-brace)
  (define-key map (kbd "C-z p a") 'jacob-xah-insert-angled-bracket)
  (define-key map (kbd "C-z p q") 'jacob-xah-insert-ascii-double-quote)
  (define-key map (kbd "C-z p s") 'jacob-xah-insert-ascii-single-quote)
  (define-key map (kbd "M-=") 'jacob-count-words-region)
  (define-key map (kbd "C-z C-l t") 'jacob-recenter-top)
  (define-key map (kbd "C-z C-l c") 'jacob-recenter-centre)
  (define-key map (kbd "C-z C-l b") 'jacob-recenter-bottom)
  (define-key map (kbd "C-z M-r t") 'jacob-move-to-window-line-top)
  (define-key map (kbd "C-z M-r c") 'jacob-move-to-window-line-centre)
  (define-key map (kbd "C-z M-r b") 'jacob-move-to-window-line-bottom)
  (jacob-is-installed 'consult
    (define-key map (kbd "C-z SPC v") 'consult-yank-from-kill-ring)
    (define-key map (kbd "C-z SPC i j") 'consult-recent-file)
    (define-key map (kbd "C-z SPC e c f") 'consult-buffer)
    (define-key map (kbd "C-z SPC e c n") 'consult-line))
  (jacob-is-installed 'projectile
    (define-key map (kbd "C-z SPC e p f") 'projectile-find-file))
  (define-key map (kbd "C-z t") 'jacob-voice-mark-command)
  (define-key map (kbd "C-x 2") 'jacob-split-window-below-select-new)
  (define-key map (kbd "C-x 3") 'jacob-split-window-right-select-new)
  (define-key map (kbd "C-z f") 'jacob-switch-to-previous-buffer)
  (define-key map (kbd "C-z F") 'ibuffer)
  (jacob-is-installed 'goto-last-change
    (define-key map (kbd "C-z j") 'goto-last-change)
    (define-key map (kbd "C-z l") 'goto-last-change-reverse)))

(jacob-is-installed 'xah-fly-keys
  (define-prefix-command 'jacob-config-keymap)
  (define-prefix-command 'jacob-eglot-keymap)

  (jacob-is-installed 'eglot
    (let ((map jacob-eglot-keymap))
      (define-key map (kbd "a") 'eglot-code-actions)
      (define-key map (kbd "r") 'eglot-rename)
      (define-key map (kbd "d") 'xref-find-definitions)
      (define-key map (kbd "u") 'xref-find-references)))

  (let ((map xah-fly-dot-keymap))
    (define-key map (kbd "v") vc-prefix-map)
    (define-key map (kbd "c") jacob-config-keymap)
    (jacob-is-installed 'eglot
      (define-key map (kbd "e") jacob-eglot-keymap))
    (jacob-is-installed 'consult
      (define-key map (kbd "s") 'consult-line))
    ;; (define-key map (kbd "p") project-prefix-map)
    (jacob-is-installed 'projectile
      (define-key map (kbd "p") 'projectile-command-map)))


  (let ((map xah-fly-command-map))
    (define-key map (kbd "a") 'execute-extended-command)
    (define-key map (kbd "s") (kbd "RET"))
    (define-key map (kbd "DEL") nil)
    (define-key map (kbd "j") (kbd "C-b"))
    (define-key map (kbd "l") (kbd "C-f"))
    (define-key map (kbd "i") (kbd "C-p"))
    (define-key map (kbd "k") (kbd "C-n"))
    (define-key map (kbd "4") 'jacob-split-window-below-select-new)
    (define-key map (kbd "2") 'jacob-quit-popup-window)
    (jacob-is-installed 'expand-region
      (define-key map (kbd "8") 'er/expand-region))
    (jacob-is-installed 'consult
      (define-key map (kbd "v") 'consult-yank)
      (define-key map (kbd "f") 'consult-buffer)))

  (let ((map jacob-config-keymap))
    (define-key map (kbd "r") 'jacob-config-reload)
    (define-key map (kbd "e") 'jacob-config-visit)
    (define-key map (kbd "c") 'jacob-org-src-block)
    (define-key map (kbd "p") 'jacob-recompile-packages)
    (define-key map (kbd "t") 'jacob-display-time)
    (jacob-is-installed 'restart-emacs
      (define-key map (kbd "R") 'restart-emacs)))

  (let ((map xah-fly-e-keymap))
    (define-key map (kbd "k") 'jacob-xah-insert-paren)
    (define-key map (kbd "l") 'jacob-xah-insert-square-bracket)
    (define-key map (kbd "j") 'jacob-xah-insert-brace)
    (define-key map (kbd "u") 'jacob-xah-insert-ascii-double-quote)
    (define-key map (kbd "i") 'jacob-xah-insert-ascii-single-quote)
    (define-key map (kbd "m") 'xah-insert-hyphen)
    (define-key map (kbd ",") 'xah-insert-low-line)
    (define-key map (kbd ".") 'jacob-insert-equals)
    (define-key map (kbd "/") 'jacob-insert-plus)
    (define-key map (kbd "z") 'jacob-insert-apostrophe)
    (define-key map (kbd "x") 'jacob-insert-at)
    (define-key map (kbd "c") 'jacob-insert-hash)
    (define-key map (kbd "d") (kbd "DEL"))
    (define-key map (kbd "v") 'jacob-insert-tilde)
    (define-key map (kbd "e") 'jacob-insert-dollar-sign)
    (define-key map (kbd "r") 'jacob-insert-caret)
    (define-key map (kbd "o") 'jacob-insert-ampersand))

  (let ((map xah-fly-leader-key-map))
    (define-key map (kbd "4") 'jacob-split-window-right-select-new))

  (let ((map xah-fly-w-keymap))
    (define-key map (kbd "n") 'jacob-eval-and-replace))

  (let ((map xah-fly-t-keymap))
    (define-key map (kbd "j") 'kill-this-buffer))

  (let ((map xah-fly-c-keymap))
    (define-key xah-fly-c-keymap (kbd "j") 'consult-recent-file)))


