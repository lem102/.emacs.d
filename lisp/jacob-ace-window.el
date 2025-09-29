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

(with-eval-after-load "xah-fly-keys"
  (keymap-set xah-fly-command-map "," #'jacob-split-or-switch-window))

(use-package ace-window
  :defer t
  :custom
  (aw-keys '(?a ?s ?d ?f ?q ?w ?e ?r))
  (aw-minibuffer-flag t)
  (aw-scope 'frame)
  (aw-dispatch-when-more-than 3))

(provide 'jacob-ace-window)

;;; jacob-ace-window.el ends here
