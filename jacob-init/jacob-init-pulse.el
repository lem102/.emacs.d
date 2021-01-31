(require 'pulse)

(defun jacob-pulse-line (&rest _)
  "Pulse the current line."
  (pulse-momentary-highlight-region (+ (line-beginning-position) (current-indentation)) (line-end-position)))

(dolist (command '(recenter-top-bottom
                   scroll-up-command
                   scroll-down-command
                   other-window))
  (advice-add command :after #'jacob-pulse-line))
