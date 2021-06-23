;;; jacob-init-minibuffer-completion --- set up minibuffer completion -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(jacob-try-require 'selectrum

  (jacob-try-require 'orderless
    (setq completion-styles '(orderless)))

  (jacob-try-require 'prescient
    (jacob-try-require 'selectrum-prescient
      (selectrum-prescient-mode 1))
    (prescient-persist-mode 1))

  (jacob-try-require 'marginalia
    (marginalia-mode 1))

  (setq selectrum-display-action nil)
  (setq selectrum-max-window-height 25)
  (setq enable-recursive-minibuffers t)
  (selectrum-mode 1))

(jacob-is-installed 'consult
  (with-eval-after-load 'consult
    (setq completion-in-region-function 'consult-completion-in-region)
    (setq consult-preview-key 'any)
    (dolist (command '(consult-bookmark consult-recent-file consult-buffer))
      (setf (alist-get command consult-config) `(:preview-key ,nil))))

  (global-set-key (kbd "C-z SPC e c f") 'consult-buffer)
  (global-set-key (kbd "C-z SPC e c n") 'consult-line))

(jacob-is-installed 'xah-fly-keys
  (with-eval-after-load 'xah-fly-keys
    (define-key xah-fly-dot-keymap (kbd "s") 'consult-line)
    (define-key xah-fly-c-keymap (kbd "j") 'consult-recent-file)
    (define-key xah-fly-leader-key-map (kbd "v") 'consult-yank)
    (define-key xah-fly-leader-key-map (kbd "f") 'consult-buffer)))

(provide 'jacob-init-minibuffer-completion)

;;; jacob-init-minibuffer-completion.el ends here
