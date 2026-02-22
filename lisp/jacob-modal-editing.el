;;; jacob-modal-editing.el --- Modal editing  -*- lexical-binding: t; -*-

;;; Commentary:
;;

;; FIXME: leaving embark can cause issues

;; theory: `embark' temporarily binds `overriding-terminal-local-map'.
;; is it possible that a call we make to alter the
;; `overriding-terminal-local-map' is mixing with embark in an
;; unexpected way?

;;; Code:

(defvar-keymap jacob-modal-editing-keymap)

(defvar-keymap jacob-modal-editing-mode-keymap)

(defvar-keymap jacob-modal-editing--internal-command-state)

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
  ;; its a wild world out there. we need to watch for other packages
  ;; touching `overriding-terminal-local-map'.

  ;; one approach is to try to leave `overriding-terminal-local-map'
  ;; alone when those packages are doing their thing.

  ;; another is to try to repair `overriding-terminal-local-map'. This
  ;; could be adding keys when we should be in command state, or
  ;; removing duplicate keys, or removing all keys if we should be in
  ;; insert state.

  ;; going to try the former. we need this function to not proceed
  ;; when the `overriding-terminal-local-map' is not in a state that we expect.

  ;; what are states that we expect?
  ;; - nil (for when we aren't in the command state)
  ;; - command state keymap (for when we are in the command state). this shouldn't be exclusive



  
  ;; lets try a hack. if an embark key (embark-cycle) is bound in
  ;; `overriding-terminal-local-map', stop here.


  (let ((in-embark (seq-find (lambda (e)
                               (equal 'embark-cycle e))
                             (flatten-tree overriding-terminal-local-map))))

    ;; the hack works. what can i do to make it more robust?

    

    

    (when (and (jacob-modal-editing-command-state-active-p)
               ;; (not in-embark)
               )
      (jacob-modal-editing--activate-command-state))))

(defun jacob-modal-editing--deactivate-command-state ()
  "Deactivate command state."
  (when jacob-modal-editing--deactivate-function
    (setq jacob-modal-editing--internal-command-state nil)
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
  (let ((map (jacob-modal-editing--build-keymap)))
    (setq jacob-modal-editing--internal-command-state map)
    (setq jacob-modal-editing--deactivate-function
          (set-transient-map map
                             (lambda () t)))))

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
