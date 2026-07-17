;;; jacob-modal-editing.el --- Modal editing  -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:

(defvar-keymap jacob-modal-editing-command-mode-map
  :doc "Keymap for command mode.
- Used to construct `jacob-modal-editing--internal-map'
- This keymap is not activated alongside `jacob-modal-editing-command-mode'")

(defvar-keymap jacob-modal-editing-mode-map
  :doc "Keymap for `jacob-modal-editing-mode'.")

(defvar-keymap jacob-modal-editing--internal-map
  :doc "Internal keymap. Do not edit. Contains composition of:
- `jacob-modal-editing-command-mode-map'
- elements of `jacob-modal-editing-overriding-map-alist'")

(defvar jacob-modal-editing--map-alist `((jacob-modal-editing-command-mode . ,jacob-modal-editing--internal-map))
  "Keymap alist which:
- Associates `jacob-modal-editing-command-mode' with
  `jacob-modal-editing--internal-map'.
- Used in `emulation-mode-map-alists' to make
  `jacob-modal-editing--internal-map' active.")

(defvar jacob-modal-editing-overriding-map-alist nil
  "Alist of major or minor modes and keymaps.
Used to construct `jacob-modal-editing--internal-map'.")

(defun jacob-modal-editing-command-mode-activate ()
  "Activate `jacob-modal-editing-command-mode'."
  (interactive)
  (jacob-modal-editing-command-mode 1))

(defun jacob-modal-editing-command-mode-deactivate ()
  "Deactivate `jacob-modal-editing-command-mode'."
  (interactive)
  (jacob-modal-editing-command-mode 0))

(defun jacob-modal-editing--build-keymap ()
  "Construct the keymap used in command mode.
Result is coposition of:
- `jacob-modal-editing-command-mode-map'
- elements of `jacob-modal-editing--map-alist'"
  (let* ((modes (with-current-buffer (window-buffer (selected-window))
                  (append local-minor-modes global-minor-modes (list major-mode))))
         (mode-keymaps (seq-keep (lambda (m)
                                   (alist-get m jacob-modal-editing-overriding-map-alist))
                                 modes)))
    (make-composed-keymap mode-keymaps jacob-modal-editing-command-mode-map)))

(defun jacob-modal-editing--update-keymap (&rest _parameters)
  "Update `jacob-modal-editing--internal-map'.

Intended to be called by hooks, so ignores arguments."
  (set-keymap-parent jacob-modal-editing--internal-map
                     (jacob-modal-editing--build-keymap)))

(define-minor-mode jacob-modal-editing-mode
  "Simple modal editing.

Allows for major mode specific commands without too much nonsense."
  :global t
  :init-value nil
  :lighter " jme"
  (jacob-modal-editing-command-mode (if jacob-modal-editing-mode 1 0)))

(define-minor-mode jacob-modal-editing-command-mode
  "Command mode for `jacob-modal-editing'.

- Setup hooks to ensure command mode keymap aligns with active modes.
- Modify `emulation-mode-map-alists' to control command mode keymap activation."
  :global t
  :init-value nil
  :lighter " jmec"
  :keymap nil
  (if jacob-modal-editing-command-mode
      (progn
        (jacob-modal-editing--update-keymap)
        (add-hook 'window-state-change-functions #'jacob-modal-editing--update-keymap)
        (add-hook 'change-major-mode-hook #'jacob-modal-editing--update-keymap)
        (add-to-list 'emulation-mode-map-alists 'jacob-modal-editing--map-alist))
    (remove-hook 'window-state-change-functions #'jacob-modal-editing--update-keymap)
    (remove-hook 'change-major-mode-hook #'jacob-modal-editing--update-keymap)
    (setq emulation-mode-map-alists (remove 'jacob-modal-editing--map-alist emulation-mode-map-alists))))

(provide 'jacob-modal-editing)

;;; jacob-modal-editing.el ends here
