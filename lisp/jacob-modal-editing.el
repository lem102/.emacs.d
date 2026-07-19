;;; jacob-modal-editing.el --- Modal editing  -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:

(defvar-keymap jme-command-mode-map
  :doc "Keymap for command mode.
- Used to construct `jacob-modal-editing--internal-map'
- This keymap is not activated alongside `jacob-modal-editing-command-mode'")

(defvar-keymap jme-mode-map
  :doc "Keymap for `jacob-modal-editing-mode'.")

(defvar-keymap jme--internal-map
  :doc "Internal keymap. Do not edit. Contains composition of:
- `jacob-modal-editing-command-mode-map'
- elements of `jacob-modal-editing-overriding-map-alist'")

(defvar jme--map-alist `((jme-command-mode . ,jme--internal-map))
  "Keymap alist which:
- Associates `jacob-modal-editing-command-mode' with
  `jacob-modal-editing--internal-map'.
- Used in `emulation-mode-map-alists' to make
  `jacob-modal-editing--internal-map' active.")

(defvar jme-overriding-map-alist nil
  "Alist of major or minor modes and keymaps.
Used to construct `jacob-modal-editing--internal-map'.")

(defun jme-command-mode-activate ()
  "Activate `jacob-modal-editing-command-mode'."
  (interactive)
  (jme-command-mode 1))

(defun jme-command-mode-deactivate ()
  "Deactivate `jacob-modal-editing-command-mode'."
  (interactive)
  (jme-command-mode 0))

(defun jme--build-keymap ()
  "Construct the keymap used in command mode.
Result is coposition of:
- `jacob-modal-editing-command-mode-map'
- elements of `jacob-modal-editing--map-alist'"
  (let* ((modes (with-current-buffer (window-buffer (selected-window))
                  (append local-minor-modes global-minor-modes (list major-mode))))
         (mode-keymaps (seq-keep (lambda (m)
                                   (alist-get m jme-overriding-map-alist))
                                 modes)))
    (make-composed-keymap mode-keymaps jme-command-mode-map)))

(defun jme--update-keymap (&rest _parameters)
  "Update `jme--internal-map'.

Intended to be called by hooks, so ignores arguments."
  (set-keymap-parent jme--internal-map
                     (jme--build-keymap)))

(define-minor-mode jme-mode
  "Simple modal editing.

Allows for major mode specific commands without too much nonsense."
  :global t
  :init-value nil
  :lighter " jme"
  (jme-command-mode (if jme-mode 1 0)))

(define-minor-mode jme-command-mode
  "Command mode for `jacob-modal-editing'.

- Setup hooks to ensure command mode keymap aligns with active modes.
- Modify `emulation-mode-map-alists' to control command mode keymap activation."
  :global t
  :init-value nil
  :lighter " jmec"
  :keymap nil
  (if jme-command-mode
      (progn
        (jme--update-keymap)
        (add-hook 'window-state-change-functions #'jme--update-keymap)
        (add-hook 'change-major-mode-hook #'jme--update-keymap)
        (add-to-list 'emulation-mode-map-alists 'jme--map-alist))
    (remove-hook 'window-state-change-functions #'jme--update-keymap)
    (remove-hook 'change-major-mode-hook #'jme--update-keymap)
    (setq emulation-mode-map-alists (remove 'jme--map-alist emulation-mode-map-alists))))

(provide 'jacob-modal-editing)

;;; jacob-modal-editing.el ends here

;; Local Variables:
;; read-symbol-shorthands: (("jme-" . "jacob-modal-editing-"))
;; End:
