;; -*- lexical-binding: t -*-
(with-eval-after-load 'dired
  (setq dired-recursive-copies 'always)
  (setq dired-recursive-deletes 'top)
  (setq dired-dwim-target t)

  (defun jacob-dired-mode-setup ()
    "hook function for dired."
    (dired-hide-details-mode 1))

  (add-hook 'dired-mode-hook 'jacob-dired-mode-setup)

  (with-eval-after-load 'dired-x
    (setq dired-guess-shell-alist-user '(("\\.mkv\\'" "mpv")))))

(provide 'jacob-init-dired)
