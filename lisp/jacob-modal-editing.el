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
  "Enable modal editing."
  (interactive)
  (when (equal (current-buffer) (window-buffer (selected-window)))
    (jacob-modal-editing t)))

(defun jacob-modal-editing-disable ()
  "Disable modal editing."
  (interactive)
  (jacob-modal-editing nil))

(defun jacob-modal-editing-map-active-p ()
  "Return non-nil if the modal editing map is active."
  (not (null jacob-modal-editing--deactivate-function)))

(defun jacob-modal-editing (enable)
  "Enable or disable command mode.

If ENABLE is non-nil command mode will be activated, otherwise it will
be deactivated."
  (interactive)
  (cond (enable
	     (let ((map (make-composed-keymap (alist-get major-mode
						                             jacob-modal-editing-major-mode-keymap-alist)
					                      jacob-modal-editing-keymap)))
           (when jacob-modal-editing--deactivate-function
             (funcall jacob-modal-editing--deactivate-function))
	       (setq jacob-modal-editing--deactivate-function
		         (set-transient-map map (lambda () t)))))
	    (t
	     (when jacob-modal-editing--deactivate-function
	       (funcall jacob-modal-editing--deactivate-function))))
  (run-hook-with-args 'jacob-modal-editing-hook enable))

(define-minor-mode jacob-modal-editing-mode
  "Simple modal editing mode that allows for major mode specific commands
without too much nonsense."
  :global t
  :init-value nil
  :lighter " jme"
  :keymap jacob-modal-editing-mode-keymap
  (if jacob-modal-editing-mode
      (progn
        (add-hook 'change-major-mode-after-body-hook #'jacob-modal-editing-enable)
        (add-hook 'window-selection-change-functions #'jacob-modal-editing-enable)
        (add-hook 'isearch-mode-end-hook #'jacob-modal-editing-enable)
        (jacob-modal-editing-enable))
    (when jacob-modal-editing--deactivate-function
      (funcall jacob-modal-editing--deactivate-function))
    (remove-hook 'change-major-mode-after-body-hook #'jacob-modal-editing-enable)
    (remove-hook 'window-selection-change-functions #'jacob-modal-editing-enable)
    (remove-hook 'isearch-mode-end-hook #'jacob-modal-editing-enable)))

(provide 'jacob-modal-editing)

;;; jacob-modal-editing.el ends here
