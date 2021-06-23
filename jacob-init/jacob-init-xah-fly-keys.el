;;; jacob-init-xah-fly-keys.el --- My configuration for Xah Fly Keys.

;;; Commentary:
;; 

;;; Code:

(define-prefix-command 'jacob-config-keymap)
(setq xah-fly-use-control-key nil)
(setq xah-fly-use-meta-key t)

(jacob-try-require 'xah-fly-keys
  (xah-fly-keys-set-layout "qwerty")
  (xah-fly-keys 1)

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

  (let ((map xah-fly-dot-keymap))
    ;; (define-key map (kbd "p") project-prefix-map)
    (define-key map (kbd "v") vc-prefix-map)
    (define-key map (kbd "c") jacob-config-keymap))

  (unbind-key (kbd "DEL") 'xah-fly-command-map)

  (global-set-key (kbd "C-z r") 'xah-jacob-kill-word)
  (global-set-key (kbd "C-z e") 'xah-jacob-backward-kill-word)
  (global-set-key (kbd "C-z h") 'xah-jacob-beginning-of-line-or-block)
  (global-set-key (kbd "C-z ;") 'xah-jacob-end-of-line-or-block)
  (global-set-key (kbd "C-z c") 'xah-copy-line-or-region)
  (global-set-key (kbd "C-z x") 'xah-cut-line-or-region)
  (global-set-key (kbd "C-z .") 'xah-forward-right-bracket)
  (global-set-key (kbd "C-z m") 'xah-backward-left-bracket)
  (global-set-key (kbd "C-z /") 'xah-goto-matching-bracket)
  (global-set-key (kbd "C-z d") 'xah-delete-backward-char-or-bracket-text)
  (global-set-key (kbd "C-z 0") 'xah-pop-local-mark-ring)
  (global-set-key (kbd "C-z v") 'xah-paste-or-paste-previous)
  (global-set-key (kbd "C-z w") 'xah-shrink-whitespaces)
  (global-set-key (kbd "C-z p p") 'jacob-xah-insert-paren)
  (global-set-key (kbd "C-z p b") 'jacob-xah-insert-square-bracket)
  (global-set-key (kbd "C-z p c") 'jacob-xah-insert-brace)
  (global-set-key (kbd "C-z p a") 'jacob-xah-insert-angled-bracket)
  (global-set-key (kbd "C-z p q") 'jacob-xah-insert-ascii-double-quote)
  (global-set-key (kbd "C-z p s") 'jacob-xah-insert-ascii-single-quote)
  
  (let ((map xah-fly-command-map))
    (define-key map (kbd "a") 'execute-extended-command)
    (define-key map (kbd "s") (kbd "RET"))
    (define-key map (kbd "4") 'jacob-split-window-below-select-new)
    (define-key map (kbd "2") 'jacob-quit-popup-window))
  
  (let ((map jacob-config-keymap))
    (define-key map (kbd "r") 'jacob-config-reload)
    (define-key map (kbd "e") 'jacob-config-visit)
    (define-key map (kbd "c") 'jacob-org-src-block)
    (define-key map (kbd "p") 'jacob-recompile-packages)
    (define-key map (kbd "t") 'jacob-display-time))
  
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
    (define-key map (kbd "j") 'kill-this-buffer)))

  (let ((map xah-fly-r-keymap)) 
    (define-key map (kbd "c") 'kmacro-set-counter))

(provide 'jacob-init-xah-fly-keys)

;;; jacob-init-xah-fly-keys.el ends here
