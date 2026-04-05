;;; jacob-ace-window.el --- Configuration for the ace-window package

;;; Commentary:
;;

;;; Code:

(defun jacob-split-or-switch-window ()
  "Split or switch window.

  If there is only one window in the current frame, split the frame and
  move to the new window. Otherwise, call `switch-buffer'."
  (interactive)
  (cond ((= 1 (let ((total-windows 0))
                (dolist (frame (frame-list))
                  (setq total-windows (+ total-windows (length (window-list frame)))))
                total-windows))
         (split-window-sensibly)
         (call-interactively #'other-window))
        (t (call-interactively #'ace-window))))

(provide 'jacob-ace-window)

;;; jacob-ace-window.el ends here
