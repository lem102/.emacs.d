(defun jacob-recenter-top ()
  (interactive)
  (recenter 5))

(defun jacob-recenter-centre ()
  (interactive)
  (recenter))

(defun jacob-recenter-bottom ()
  (interactive)
  (recenter -5))

(defun jacob-move-to-window-line-top ()
  (interactive)
  (move-to-window-line 5))

(defun jacob-move-to-window-line-centre ()
  (interactive)
  (move-to-window-line nil))

(defun jacob-move-to-window-line-bottom ()
  (interactive)
  (move-to-window-line -5))

(defun jacob-voice-mark-command ()
  (interactive)
  (if (region-active-p)
      (er/expand-region 1)
    (set-mark (point))))

(defun jacob-switch-to-previous-buffer ()
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))
