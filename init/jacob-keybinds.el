;;; jacob-keybinds.el --- my keybindings

;;; Commentary:
;; 

;;; Code:


;; macros

(fset 'jacob-return-macro [return])


;; voice command keybindings

(global-unset-key (kbd "C-z"))

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
  (define-key map (kbd "C-z t") 'jacob-voice-mark-command)
  (define-key map (kbd "C-x 2") 'jacob-split-window-below-select-new)
  (define-key map (kbd "C-x 3") 'jacob-split-window-right-select-new)
  (define-key map (kbd "C-z f") 'jacob-switch-to-previous-buffer)
  (define-key map (kbd "C-z F") 'ibuffer)
  (jacob-is-installed 'goto-last-change
    (define-key map (kbd "C-z j") 'goto-last-change)
    (define-key map (kbd "C-z l") 'goto-last-change-reverse)))


;; xah-fly-keys keybindings

(jacob-is-installed 'xah-fly-keys
  (define-prefix-command 'jacob-config-keymap)
  (define-prefix-command 'jacob-eglot-keymap)

  (jacob-is-installed 'eglot
    (let ((map jacob-eglot-keymap))
      (define-key map "a" 'eglot-code-actions)
      (define-key map "r" 'eglot-rename)))

  (let ((map xah-fly-dot-keymap))
    (define-key map "v" vc-prefix-map)
    (define-key map "t" tab-prefix-map)
    (define-key map "c" jacob-config-keymap)
    (define-key map "p" project-prefix-map)
    (jacob-is-installed 'eglot
      (define-key map "e" jacob-eglot-keymap))
    (jacob-is-installed 'consult
      (define-key map "s" 'consult-line))
    (let ((map project-prefix-map))
      (define-key map "g" 'jacob-project-search))
    (define-key map "v" vc-prefix-map)
    (define-key map "b" 'modus-themes-toggle))

  (let ((map xah-fly-command-map))
    (define-key map "a" 'execute-extended-command)
    (define-key map "s" 'jacob-return-macro)
    (define-key map "DEL" nil)
    ;; (define-key map "4" 'jacob-split-window-right-select-new)
    (define-key map "4" 'other-window-prefix)
    (define-key map "1" 'winner-undo)
    (define-key map "2" 'winner-redo)
    (define-key map "9" 'jacob-swap-visible-buffers)
    (define-key map "'" 'jacob-format-words-3)
    (jacob-is-installed 'expand-region
      (define-key map "8" 'er/expand-region)))

  (let ((map jacob-config-keymap))
    (define-key map "r" 'jacob-config-reload)
    (define-key map "e" 'jacob-config-visit)
    (define-key map "c" 'jacob-org-src-block)
    (define-key map "p" 'jacob-recompile-packages)
    (define-key map "t" 'jacob-display-time)
    (jacob-is-installed 'restart-emacs
      (define-key map "R" 'restart-emacs)))

  (defvar jacob-insert-parentheses-character ?k)
  (defvar jacob-insert-square-bracket-character ?l)
  (defvar jacob-insert-curly-brace-character ?j)
  (defvar jacob-insert-double-quote-character ?u)
  (defvar jacob-insert-single-quote-character ?i)
  (defvar jacob-insert-angle-bracket-character ?h)

  (setq insert-pair-alist `((,jacob-insert-parentheses-character ?\( ?\))
                            (,jacob-insert-square-bracket-character ?\[ ?\])
                            (,jacob-insert-curly-brace-character ?\{ ?\})
                            (,jacob-insert-double-quote-character ?\" ?\")
                            (,jacob-insert-single-quote-character ?\' ?\')
                            (,jacob-insert-angle-bracket-character ?\< ?\>)))

  (let ((map xah-fly-e-keymap))
    (define-key map (char-to-string jacob-insert-parentheses-character) 'insert-pair)
    (define-key map (char-to-string jacob-insert-square-bracket-character) 'insert-pair)
    (define-key map (char-to-string jacob-insert-curly-brace-character) 'insert-pair)
    (define-key map (char-to-string jacob-insert-double-quote-character) 'insert-pair)
    (define-key map (char-to-string jacob-insert-single-quote-character) 'insert-pair)
    (define-key map (char-to-string jacob-insert-angle-bracket-character) 'insert-pair)
    (define-key map "m" 'xah-insert-hyphen)
    (define-key map "," 'xah-insert-low-line)
    (define-key map "." 'jacob-insert-equals)
    (define-key map "/" 'jacob-insert-plus)
    (define-key map "z" 'jacob-insert-apostrophe)
    (define-key map "x" 'jacob-insert-at)
    (define-key map "c" 'jacob-insert-hash)
    (define-key map (kbd "d") 'backward-delete-char)
    (define-key map "v" 'jacob-insert-tilde)
    (define-key map "e" 'jacob-insert-dollar-sign)
    (define-key map "r" 'jacob-insert-caret)
    (define-key map "o" 'jacob-insert-ampersand))

  (defvar jacob-recenter-repeat-map
    (let ((map (make-sparse-keymap)))
      (define-key map "p" 'recenter-top-bottom)
      map))

  (put 'recenter-top-bottom 'repeat-map 'jacob-recenter-repeat-map)

  (let ((map xah-fly-leader-key-map))
    (jacob-is-installed 'consult
      (define-key map "v" 'consult-yank-from-kill-ring)
      (define-key map "f" 'consult-buffer)))

  (let ((map xah-fly-w-keymap))
    (define-key map "n" 'jacob-eval-and-replace))

  (let ((map xah-fly-t-keymap))
    (define-key map "j" 'xah-close-current-buffer))

  (let ((map xah-fly-c-keymap))
    (define-key map "j" 'consult-recent-file)
    (define-key map "e" 'find-file))

  (let ((map xah-fly-t-keymap))
    (define-key map "j" 'kill-current-buffer))

  (let ((map xah-fly-r-keymap))
    (define-key map "c" 'kmacro-set-counter))

  (let ((map xah-fly-n-keymap))
    (define-key map "a" 'jacob-font-size-increase)
    (define-key map "3" 'jacob-async-shell-command))

  (let ((map vc-prefix-map))
    (define-key map "p" 'vc-push))

  (let ((map minibuffer-local-completion-map))
    (define-key map "SPC" 'self-insert-command))

  (let ((map dired-mode-map))
    (jacob-xah-define-key map "q" 'quit-window)
    (jacob-xah-define-key map "i" 'dired-previous-line)
    (jacob-xah-define-key map "k" 'dired-next-line)
    (jacob-xah-define-key map "s" 'dired-find-file)
    (jacob-xah-define-key map "e" 'dired-mark)
    (jacob-xah-define-key map "r" 'dired-unmark)
    (jacob-xah-define-key map "x" 'dired-do-rename)
    (jacob-xah-define-key map "c" 'dired-do-copy)
    (jacob-xah-define-key map "d" 'dired-do-delete) ; we skip the "flag, delete" process as files are sent to system bin on deletion
    (jacob-xah-define-key map "u" 'dired-up-directory)
    (jacob-xah-define-key map "j" 'dired-goto-file))

  (let ((map occur-mode-map))
    (jacob-xah-define-key map "q" 'quit-window)
    (jacob-xah-define-key map "i" 'previous-error-no-select)
    (jacob-xah-define-key map "k" 'next-error-no-select))

  (with-eval-after-load 'vc-dir
    (let ((map vc-dir-mode-map))
      (jacob-xah-define-key map "q" 'quit-window)
      (jacob-xah-define-key map "i" 'vc-dir-previous-line)
      (jacob-xah-define-key map "k" 'vc-dir-next-line)
      (jacob-xah-define-key map "o" 'vc-dir-next-directory)
      (jacob-xah-define-key map "u" 'vc-dir-previous-directory)
      (jacob-xah-define-key map "s" 'vc-dir-find-file)
      (jacob-xah-define-key map "e" 'vc-dir-mark)
      (jacob-xah-define-key map "r" 'vc-dir-unmark)
      (jacob-xah-define-key map "v" 'vc-next-action)
      (jacob-xah-define-key map "p" 'vc-push)))

  (with-eval-after-load 'info
    (let ((map Info-mode-map))
      (jacob-xah-define-key map "q" 'quit-window)
      (jacob-xah-define-key map "l" 'Info-scroll-up)
      (jacob-xah-define-key map "j" 'Info-scroll-down)
      (jacob-xah-define-key map "i" 'Info-up)
      (jacob-xah-define-key map "k" 'Info-menu)))

  (with-eval-after-load 'calendar
    (let ((map calendar-mode-map))
      (jacob-xah-define-key map "q" 'quit-window)
      (jacob-xah-define-key map "i" 'calendar-backward-week)
      (jacob-xah-define-key map "k" 'calendar-forward-week)
      (jacob-xah-define-key map "j" 'calendar-backward-day)
      (jacob-xah-define-key map "l" 'calendar-forward-day)
      (jacob-xah-define-key map "u" 'calendar-backward-month)
      (jacob-xah-define-key map "o" 'calendar-forward-month)
      (jacob-xah-define-key map "d" 'diary-view-entries)
      (jacob-xah-define-key map "s" 'diary-insert-entry)
      (jacob-xah-define-key map "m" 'diary-mark-entries)
      (jacob-xah-define-key map "." 'calendar-goto-today)
      (jacob-xah-define-key map "t" 'calendar-set-mark)))

  (with-eval-after-load 'doc-view
    (let ((map doc-view-mode-map))
      (jacob-xah-define-key map "l" 'doc-view-next-page)
      (jacob-xah-define-key map "j" 'doc-view-previous-page)))

  (with-eval-after-load 'diff-mode
    (let ((map diff-mode-map))
      (jacob-xah-define-key map "q" 'quit-window))))


(with-eval-after-load 'smerge
  (defvar jacob-smerge-repeat-map
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "n") 'smerge-next)
      (define-key map (kbd "p") 'smerge-prev)
      (define-key map (kbd "u") 'smerge-keep-upper)
      (define-key map (kbd "l") 'smerge-keep-lower)
      map))
  
  (put 'smerge-next 'repeat-map 'jacob-smerge-repeat-map)
  (put 'smerge-prev 'repeat-map 'jacob-smerge-repeat-map)
  (put 'smerge-keep-upper 'repeat-map 'jacob-smerge-repeat-map)
  (put 'smerge-keep-lower 'repeat-map 'jacob-smerge-repeat-map))

(provide 'jacob-keybinds)

;;; jacob-keybinds.el ends here
