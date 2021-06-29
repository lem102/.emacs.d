;;; jacob-init-xah-fly-keys.el --- My configuration for Xah Fly Keys.

;;; Commentary:
;; 

;;; Code:

(setq xah-fly-use-control-key nil)
(setq xah-fly-use-meta-key t)

(jacob-try-require 'xah-fly-keys
  (xah-fly-keys-set-layout "qwerty")
  (xah-fly-keys 1)

  (add-hook 'after-init-hook (lambda () (global-set-key (kbd "C-<kp-subtract>") nil) (define-key global-map (kbd "C-<kp-subtract> C-`") 'jacob-insert-asterisk)))

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
      (xah-end-of-line-or-block))))

  (let ((map xah-fly-r-keymap)) 
    (define-key map (kbd "c") 'kmacro-set-counter))

(provide 'jacob-init-xah-fly-keys)

;;; jacob-init-xah-fly-keys.el ends here
