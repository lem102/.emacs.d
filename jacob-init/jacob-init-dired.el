(setq dired-recursive-copies 'always)
(setq dired-recursive-deletes 'top)
(setq dired-dwim-target t)

(defun jacob-dired-mode-setup ()
  "hook function for dired."
  (dired-hide-details-mode 1))

(add-hook 'dired-mode-hook 'jacob-dired-mode-setup)
