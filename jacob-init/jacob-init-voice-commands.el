(defun jacob-recenter-top ()
  (interactive)
  (recenter 5))

(defun jacob-recenter-centre ()
  (interactive)
  (recenter))

(defun jacob-recenter-bottom ()
  (interactive)
  (recenter -5))

(global-set-key (kbd "C-z C-l t") 'jacob-recenter-top)
(global-set-key (kbd "C-z C-l c") 'jacob-recenter-centre)
(global-set-key (kbd "C-z C-l b") 'jacob-recenter-bottom)

(defun jacob-move-to-window-line-top ()
  (interactive)
  (move-to-window-line 5))

(defun jacob-move-to-window-line-centre ()
  (interactive)
  (move-to-window-line nil))

(defun jacob-move-to-window-line-bottom ()
  (interactive)
  (move-to-window-line -5))

(global-set-key (kbd "C-z M-r t") 'jacob-move-to-window-line-top)
(global-set-key (kbd "C-z M-r c") 'jacob-move-to-window-line-centre)
(global-set-key (kbd "C-z M-r b") 'jacob-move-to-window-line-bottom)


(global-set-key (kbd "C-z SPC v") 'consult-yank-from-kill-ring)

(global-set-key (kbd "C-z SPC i j") 'consult-recent-file)

(global-set-key (kbd "C-z SPC e p f") 'projectile-find-file)

(defun jacob-voice-mark-command ()
  (interactive)
  (if (region-active-p)
      (er/expand-region 1)
    (set-mark (point))))

(global-set-key (kbd "C-z t") 'jacob-voice-mark-command)

(global-set-key (kbd "C-x 2") 'jacob-split-window-below-select-new)
(global-set-key (kbd "C-x 3") 'jacob-split-window-right-select-new)

(defun jacob-switch-to-previous-buffer ()
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

(global-set-key (kbd "C-z f") 'jacob-switch-to-previous-buffer)
(global-set-key (kbd "C-z F") 'ibuffer)
