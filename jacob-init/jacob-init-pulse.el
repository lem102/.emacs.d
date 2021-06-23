(defun jacob-pulse-line (&rest _)
  "Pulse the current line."
  (pulse-momentary-highlight-region (+ (line-beginning-position) (current-indentation)) (line-end-position)))

(dolist (command '(recenter-top-bottom
                   scroll-up-command
                   scroll-down-command
                   other-window
                   jacob-move-to-window-line-top
                   jacob-move-to-window-line-centre
                   jacob-move-to-window-line-bottom
                   jacob-recenter-top
                   jacob-recenter-centre
                   jacob-recenter-bottom
                   ))
  (advice-add command :after #'jacob-pulse-line))
