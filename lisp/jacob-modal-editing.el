;;; jacob-modal-editing.el --- Modal editing  -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:

(defvar-keymap jacob-modal-editing-keymap)

(defvar-keymap jacob-modal-editing-mode-keymap)

(defvar-keymap jacob-modal-editing--internal-keymap)

(defvar jacob-modal-editing-major-mode-keymap-alist nil)

(defun jacob-modal-editing-enable ()
  "Use keybinds from relevant `jacob-modal-editing' keymaps."
  (interactive)
  (jacob-modal-editing-command-mode 1))

(defun jacob-modal-editing-disable ()
  "Use regular keybinds."
  (interactive)
  (jacob-modal-editing-command-mode 0))

(defun jacob-modal-editing--build-keymap ()
  "Construct the keymap for command state."
  (let* ((modes (with-current-buffer (window-buffer (selected-window))
                  (append local-minor-modes global-minor-modes (list major-mode))))
         (mode-keymaps (seq-keep (lambda (m)
                                   (alist-get m jacob-modal-editing-major-mode-keymap-alist))
                                 modes)))
    (make-composed-keymap mode-keymaps jacob-modal-editing-keymap)))

(defun jacob-modal-editing--update-keymap (&rest _parameters)
  "Update the internal keymap.

Intended to be called by hooks, so takes any number of arguments and does nothing with them."
  (set-keymap-parent jacob-modal-editing--internal-keymap
                     (jacob-modal-editing--build-keymap)))

(defun jacob-modal-editing-ensure-priority (&optional _file)
  "Ensure `jacob-modal-editing' keybindings have priority over other minor modes.

Called via the `after-load-functions' special hook."
  (unless (eq (caar minor-mode-map-alist) 'jacob-modal-editing-mode)
    (let ((mykeys (assq 'jacob-modal-editing-mode minor-mode-map-alist)))
      (assq-delete-all 'jacob-modal-editing-mode minor-mode-map-alist)
      (add-to-list 'minor-mode-map-alist mykeys)))
  (unless (eq (caar minor-mode-map-alist) 'jacob-modal-editing-command-mode)
    (let ((mykeys (assq 'jacob-modal-editing-command-mode minor-mode-map-alist)))
      (assq-delete-all 'jacob-modal-editing-command-mode minor-mode-map-alist)
      (add-to-list 'minor-mode-map-alist mykeys))))

(define-minor-mode jacob-modal-editing-mode
  "Simple modal editing mode.

Allows for major mode specific commands without too much nonsense."
  :global t
  :init-value nil
  :lighter " jme"
  :keymap jacob-modal-editing-mode-keymap
  (jacob-modal-editing-ensure-priority)
  (jacob-modal-editing-command-mode (if jacob-modal-editing-mode 1 0)))

(define-minor-mode jacob-modal-editing-command-mode
  "Enable the command state."
  :global t
  :init-value nil
  :lighter " jmec"
  :keymap jacob-modal-editing--internal-keymap
  (if jacob-modal-editing-command-mode
      (progn
        (jacob-modal-editing--update-keymap)
        (add-hook 'window-state-change-functions #'jacob-modal-editing--update-keymap)
        (add-hook 'change-major-mode-hook #'jacob-modal-editing--update-keymap))
    (remove-hook 'window-state-change-functions #'jacob-modal-editing--update-keymap)
    (remove-hook 'change-major-mode-hook #'jacob-modal-editing--update-keymap)))

(provide 'jacob-modal-editing)

;;; jacob-modal-editing.el ends here
