(use-package dired
  :config
  (setq dired-recursive-copies 'always)
  (setq dired-recursive-deletes 'top)
  (setq dired-dwim-target t)

  (defun jacob-dired-mode-setup ()
    "hook function for dired."
    (dired-hide-details-mode 1))

  :hook (dired-mode-hook . jacob-dired-mode-setup))

(use-package dired-x
  :config
  (setq dired-guess-shell-alist-user '(("\\.mkv\\'" "mpv"))))
