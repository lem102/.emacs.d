;;; jacob-modal-editing.el --- Modal editing  -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:

(defvar-keymap jacob-modal-editing-keymap)

(defvar-keymap jacob-modal-editing-mode-keymap)

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
  ;; TODO: revisit this. how can i get it to work?
  (let* ((modes (with-current-buffer (window-buffer (selected-window))
                  (append local-minor-modes global-minor-modes (list major-mode))))
         (mode-keymaps (seq-keep (lambda (m)
                                   (alist-get m jacob-modal-editing-major-mode-keymap-alist))
                                 modes)))
    (make-composed-keymap mode-keymaps jacob-modal-editing-keymap)))

(define-minor-mode jacob-modal-editing-mode
  "Simple modal editing mode.

Allows for major mode specific commands without too much nonsense."
  :global t
  :init-value nil
  :lighter " jme"
  :keymap jacob-modal-editing-mode-keymap
  (jacob-modal-editing-command-mode (if jacob-modal-editing-mode 1 0)))

(define-minor-mode jacob-modal-editing-command-mode
  "Enable the command state."
  :global t
  :init-value nil
  :lighter " jmec"
  :keymap jacob-modal-editing-keymap)

(provide 'jacob-modal-editing)

;;; jacob-modal-editing.el ends here
