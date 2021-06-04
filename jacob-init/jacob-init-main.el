;;; -*- lexical-binding: t -*-

;; essential
(jacob-load "jacob-init-personal-functions.el")
(jacob-load "jacob-init-system-functions.el")
(jacob-load "jacob-init-garbage-collection.el")
(jacob-load "jacob-init-startup-time.el")
(jacob-load "jacob-init-package.el")
(jacob-load "jacob-init-local-packages.el")
(jacob-load "jacob-init-use-package.el")
(jacob-load "jacob-init-environment-setup.el")

;; keybinding
(jacob-load "jacob-init-xah-fly-keys.el")
(jacob-load "jacob-init-tabs.el")
(jacob-load "jacob-init-key-chord.el")

;; user interface
(jacob-load "jacob-init-gui-components.el") ; maybe put in misc, or investigate whether some of this should go in an early-init.el file
(jacob-load "jacob-init-dired.el")
(jacob-load "jacob-init-theme.el")
(jacob-load "jacob-init-modeline.el")
(jacob-load "jacob-init-fonts.el")
(jacob-load "jacob-init-pulse.el")
(jacob-load "jacob-init-minibuffer-completion.el")

;; miscellaneous settings
(jacob-load "jacob-init-misc.el")

;; settings for microsoft windows
(if (string-equal system-type "windows-nt")
    (progn
      (jacob-load "jacob-init-windows-settings.el")))

;; major mode packages
(jacob-load "jacob-init-elisp-mode.el")
(jacob-load "jacob-init-org-mode.el")
(jacob-load "jacob-init-yaml-mode.el")
(jacob-load "jacob-init-c-mode.el")           ; needs major review (and is, also, probably uneeded outside of educational purposes.)
(jacob-load "jacob-init-java-mode.el")
(jacob-load "jacob-init-csharp-mode.el")
(jacob-load "jacob-init-web-mode.el")
(jacob-load "jacob-init-json-mode.el")
(jacob-load "jacob-init-go-mode.el")
(jacob-load "jacob-init-powershell-mode.el")

;; minor mode packages
(jacob-load "jacob-init-eglot.el")      ; need to investigate what happens between the completion candidate being chosen and it being inserted in the buffer. 
(jacob-load "jacob-init-dimmer.el")
(jacob-load "jacob-init-flycheck.el")   ; it's a dependency of omnisharp. try to use flymake instead.
(jacob-load "jacob-init-which-key.el")
(jacob-load "jacob-init-company.el")
(jacob-load "jacob-init-projectile.el")       ; should be replaced with project.el when project.el is as good as projectile.
(jacob-load "jacob-init-omnisharp.el")        ; will replace with eglot when able.
(jacob-load "jacob-init-yasnippet.el")
(jacob-load "jacob-init-texfrag.el")

;; Non-mode Packages
(jacob-load "jacob-init-try.el")
(jacob-load "jacob-init-avy.el")
(jacob-load "jacob-init-restart-emacs.el")
(jacob-load "jacob-init-ace-window.el")       ; upgrade to frog-jump-buffer-mode.
(jacob-load "jacob-init-expand-region.el")

;; start the server
;; TODO: investigate server techniques
(jacob-load "jacob-init-server.el")
(server-start)




(jacob-load "jacob-init-voice-commands.el")
(jacob-load "jacob-init-abbrev.el")

(use-package edit-server
  :ensure t
  :config
  (edit-server-start))

(use-package ahk-mode
  :ensure t
  :mode ("\\.ahk\\$" . ahk-mode))

(jacob-load "jacob-init-auctex.el")

(use-package goto-last-change
  :bind
  ("C-z j" . goto-last-change)
  ("C-z l" . goto-last-change-reverse))
