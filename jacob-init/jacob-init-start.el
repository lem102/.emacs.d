;;; -*- lexical-binding: t -*-

;; essential
(jacob-load "jacob-init-garbage-collection.el")
(jacob-load "jacob-init-startup-time.el")
(jacob-load "jacob-init-package.el")
(jacob-load "jacob-init-use-package.el")      ; one day i should be able to remove this package from my config. (i wish)
(jacob-load "jacob-init-environment-setup.el")

;; keybinding
(jacob-load "jacob-init-xah-fly-keys.el")
(jacob-load "jacob-init-tabs.el")

;; user interface
(jacob-load "jacob-init-gui-components.el")
(jacob-load "jacob-init-dired.el")
(jacob-load "jacob-init-theme.el")
(jacob-load "jacob-init-modeline.el")
(jacob-load "jacob-init-fonts.el")
(jacob-load "jacob-init-pulse.el")

;; miscellaneous settings
(jacob-load "jacob-init-misc.el")
(jacob-load "jacob-init-windows.el")
(jacob-load "jacob-init-personal-functions.el")

;; major mode packages
(jacob-load "jacob-init-elisp-mode.el")
(jacob-load "jacob-init-org-mode.el")
(jacob-load "jacob-init-yaml-mode.el")        ; will come in.
(jacob-load "jacob-init-c-mode.el")           ; needs major review (and is, also, probably uneeded outside of educational purposes.)
(jacob-load "jacob-init-java-mode.el")
(jacob-load "jacob-init-csharp-mode.el")
(jacob-load "jacob-init-web-mode.el")         ; also important, as is used for editing razor templates.
(jacob-load "jacob-init-json-mode.el")        ; will also come in.

;; minor mode packages
(jacob-load "jacob-init-eglot.el")      ; need to investigate what happens between the completion candidate being chosen and it being inserted in the buffer. 
(jacob-load "jacob-init-dimmer.el")
(jacob-load "jacob-init-flycheck.el")
(jacob-load "jacob-init-which-key.el")
(jacob-load "jacob-init-company.el")
(jacob-load "jacob-init-projectile.el")       ; should be replaced with project.el.
(jacob-load "jacob-init-omnisharp.el")        ; will replace with eglot when able.
(jacob-load "jacob-init-yasnippet.el")
(jacob-load "jacob-init-key-chord.el")
(jacob-load "jacob-init-olivetti.el")

;; Non-mode Packages
(jacob-load "jacob-init-try.el")
(jacob-load "jacob-init-avy.el")
(jacob-load "jacob-init-restart-emacs.el")
(jacob-load "jacob-init-ace-window.el")       ; switch window might be better actally, i miss the big letters
(jacob-load "jacob-init-expand-region.el")
(jacob-load "jacob-init-shell-pop.el")        ; needs a rethink, or replacement
(jacob-load "jacob-init-amx.el")
