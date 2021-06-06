;; -*- lexical-binding: t -*-
(jacob-demand-external-package 'selectrum)
(jacob-ensure-package-installed 'consult)

(with-eval-after-load 'selectrum
  (jacob-demand-external-package 'orderless)
  (jacob-demand-external-package 'prescient)
  (jacob-demand-external-package 'selectrum-prescient)
  (jacob-demand-external-package 'marginalia)

  (with-eval-after-load 'orderless
    (setq completion-styles '(orderless)))

  (with-eval-after-load 'prescient
    (prescient-persist-mode 1))

  (with-eval-after-load 'selectrum-prescient
    (selectrum-prescient-mode 1))

  (with-eval-after-load 'marginalia
    (marginalia-mode 1))

  (setq selectrum-display-action nil)
  (setq selectrum-max-window-height 25)
  (setq enable-recursive-minibuffers t)
  (selectrum-mode 1))

(with-eval-after-load 'consult
  (setq completion-in-region-function 'consult-completion-in-region)
  (setq consult-preview-key 'any)
  (dolist (cmd '(consult-bookmark consult-recent-file consult-buffer))
    (setf (alist-get cmd consult-config) `(:preview-key ,nil))))

(global-set-key (kbd "C-z SPC e c f") 'consult-buffer)
(global-set-key (kbd "C-z SPC e c n") 'consult-line)

(with-eval-after-load 'xah-fly-keys
  (define-key xah-fly-dot-keymap (kbd "s") 'consult-line)
  (define-key xah-fly-c-keymap (kbd "j") 'consult-recent-file)
  (define-key xah-fly-leader-key-map (kbd "v") 'consult-yank))

(provide 'jacob-init-minibuffer-completion)
;;; jacob-init-minibuffer-completion.el ends here
