(require 'pulse)

(defun jacob-pulse-line (&rest _)
  "Pulse the current line."
  ;; (pulse-momentary-highlight-region (line-beginning-position line-end-position))
  (pulse-momentary-highlight-one-line (point)))

(dolist (command '(recenter-top-bottom
                   scroll-up-command
                   scroll-down-command
                   switch-window))
  (advice-add command :after #'jacob-pulse-line))
