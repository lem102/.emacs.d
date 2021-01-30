(use-package shell-pop
  :ensure t
  :config
  (setq shell-pop-autocd-to-working-dir nil)
  ;; (setq shell-pop-shell-type (quote ("eshell" "*eshell*" (lambda nil (eshell)))))
  (setq shell-pop-universal-key "<H-return>")
  (setq shell-pop-window-position "bottom")
  (setq shell-pop-window-size 50)

  (defun jacob-shell-pop-eshell ()
    (interactive)
    (let ((shell-pop-shell-type '("eshell" "*eshell*" (lambda () (eshell))))
          (shell-pop-term-shell "eshell"))
      (shell-pop--set-shell-type 'shell-pop-shell-type shell-pop-shell-type)
      (call-interactively 'shell-pop)))

  (defun jacob-shell-pop-shell ()
    (interactive)
    (let ((shell-file-name (cond
                            ((string-equal system-type "windows-nt")
                             "C:/Windows/System32/Cmd.exe")
                            ((string-equal system-type "gnu/linux")
                             "/bin/bash")))
          (shell-pop-shell-type '("shell" "*shell*" (lambda () (shell))))
          (shell-pop-term-shell "shell"))
      (shell-pop--set-shell-type 'shell-pop-shell-type shell-pop-shell-type)
      (call-interactively 'shell-pop)))
  
  :bind
  (:map xah-fly-n-keymap
        ("d" . jacob-shell-pop-eshell)
        ("f" . jacob-shell-pop-shell)))
