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
      (setf (alist-get command consult-config) `(:preview-key ,nil)))))

(provide 'jacob-init-minibuffer-completion)

;;; jacob-init-minibuffer-completion.el ends here
