;;; jacob-modal-editing.el --- Modal editing

;;; Commentary:
;;

;;; Code:

(defvar-keymap jacob-modal-editing-keymap)

(defvar-keymap jacob-modal-editing-mode-keymap)

(defvar jacob-modal-editing--deactivate-function nil)

(defvar jacob-modal-editing-major-mode-keymap-alist nil)

(defvar jacob-modal-editing-hook nil)

(defun jacob-modal-editing-enable ()
  "Use keybinds from relevant `jacob-modal-editing' keymaps."
  (interactive)
  (jacob-modal-editing--core t))

(defun jacob-modal-editing-disable ()
  "Use regular keybinds."
  (interactive)
  (jacob-modal-editing--core nil))

(defun jacob-modal-editing-command-state-active-p ()
  "Return non-nil if the command state is active."
  (not (null jacob-modal-editing--deactivate-function)))

(defun jacob-modal-editing--update-command-state (&rest _args)
  "Refresh the command state."
  (when (jacob-modal-editing-command-state-active-p)
    (jacob-modal-editing--activate-command-state)))

(defun jacob-modal-editing--deactivate-command-state ()
  "Deactivate command state."
  (when jacob-modal-editing--deactivate-function
    (funcall jacob-modal-editing--deactivate-function)
    (setq jacob-modal-editing--deactivate-function nil)))

(defun jacob-modal-editing--build-keymap ()
  "Construct the keymap for command state."
  (let* ((modes (with-current-buffer (window-buffer (selected-window))
                  (append local-minor-modes global-minor-modes (list major-mode))))
         (mode-keymaps (seq-keep (lambda (m)
                                   (alist-get m jacob-modal-editing-major-mode-keymap-alist))
                                 modes)))
    (make-composed-keymap mode-keymaps jacob-modal-editing-keymap)))

(defun jacob-modal-editing--activate-command-state ()
  "Activate command state."
  (jacob-modal-editing--deactivate-command-state)
  (setq jacob-modal-editing--deactivate-function
        (set-transient-map (jacob-modal-editing--build-keymap)
                           (lambda () t))))

(defun jacob-modal-editing--core (enable)
  "Enable or disable the command state.

If ENABLE is non-nil the command state will be activated, otherwise it
will be deactivated."
  (interactive)
  (when jacob-modal-editing-mode
    (if enable
        (jacob-modal-editing--activate-command-state)
      (jacob-modal-editing--deactivate-command-state))
    (run-hook-with-args 'jacob-modal-editing-hook enable)))

(define-minor-mode jacob-modal-editing-mode
  "Simple modal editing mode that allows for major mode specific commands
without too much nonsense."
  :global t
  :init-value nil
  :lighter " jme"
  :keymap jacob-modal-editing-mode-keymap
  (if jacob-modal-editing-mode
      (progn
        (add-hook 'after-change-major-mode-hook #'jacob-modal-editing--update-command-state)
        (add-hook 'window-state-change-functions #'jacob-modal-editing--update-command-state)
        (add-hook 'isearch-mode-end-hook #'jacob-modal-editing--update-command-state)
        (jacob-modal-editing--activate-command-state))
    (remove-hook 'after-change-major-mode-hook #'jacob-modal-editing--update-command-state)
    (remove-hook 'window-state-change-functions #'jacob-modal-editing--update-command-state)
    (remove-hook 'isearch-mode-end-hook #'jacob-modal-editing--update-command-state)
    (jacob-modal-editing--deactivate-command-state)))

(provide 'jacob-modal-editing)

;;; jacob-modal-editing.el ends here
