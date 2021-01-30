;;; -*- lexical-binding: t -*-

;; essential
(jacob-load "jacob-init-garbage-collection.el")
(jacob-load "jacob-init-startup-time.el")
(jacob-load "jacob-init-package.el")
(jacob-load "jacob-init-use-package.el")      ; one day i should be able to remove this package from my config.
(jacob-load "jacob-init-environment-setup.el")

;; keybinding
(jacob-load "jacob-init-xah-fly-keys.el")
(jacob-load "jacob-init-tabs.el")

;; user interface
(jacob-load "jacob-init-gui-components.el")
(jacob-load "jacob-init-theme.el")
(jacob-load "jacob-init-modeline.el")
(jacob-load "jacob-init-fonts.el")

;; miscellaneous settings
(jacob-load "jacob-init-misc.el")
(jacob-load "jacob-init-windows.el")
(jacob-load "jacob-init-personal-functions.el")

;; language server protocol related. when lsp-mode is gone, eglot can join its friends in the minor mode section.
(jacob-load "jacob-init-eglot.el")
(jacob-load "jacob-init-lsp-mode.el")         ; to be deprecated, use eglot only!!!

;; Major Mode Packages
;; all major mode packages need major review. need a writing style setup. activate necessary minor modes within appropriate major mode config, not in own minor mode config.
(jacob-load "jacob-init-php-mode.el")         ; will not be needed any more.
(jacob-load "jacob-init-elisp-mode.el")
(jacob-load "jacob-init-bnf-mode.el")         ; probably uneeded
(jacob-load "jacob-init-org-mode.el")
(jacob-load "jacob-init-yaml-mode.el")        ; will come in
(jacob-load "jacob-init-c-mode.el")           ; needs major review (and is, also, probably uneeded outside of educational purposes.)
(jacob-load "jacob-init-java-mode.el")
(jacob-load "jacob-init-csharp-mode.el")      ; needs urgent review, this is my job language lolololol
(jacob-load "jacob-init-web-mode.el")         ; also important, as is used for editing razor templates
(jacob-load "jacob-init-json-mode.el")        ; will also come in
(jacob-load "jacob-init-ahk-mode.el")         ; almost certainly pointless

;; Minor Mode Packages
(jacob-load "jacob-init-beacon.el")           ; another one to be removed. I want to replace this with the pulse.el library.
(jacob-load "jacob-init-dimmer.el")
(jacob-load "jacob-init-flycheck.el")
(jacob-load "jacob-init-which-key.el")
(jacob-load "jacob-init-company.el")          ; i would like to not have to use this anymore. an alternative is needed.
(jacob-load "jacob-init-projectile.el")
(jacob-load "jacob-init-omnisharp.el")        ; a disgusting, hot mess. need to follow major mode instructions above. will replace with eglot when able.
(jacob-load "jacob-init-yasnippet.el")        ; another example of a mode that should only be turned on in the major mode it is required in.
(jacob-load "jacob-init-key-chord.el")
(jacob-load "jacob-init-olivetti.el")

;; Non-mode Packages
(jacob-load "jacob-init-try.el")
(jacob-load "jacob-init-avy.el")
(jacob-load "jacob-init-restart-emacs.el")
(jacob-load "jacob-init-switch-window.el")    ; could do with a replace, kinda cumbersome
(jacob-load "jacob-init-ivy.el")              ; should replace
(jacob-load "jacob-init-expand-region.el")
(jacob-load "jacob-init-shell-pop.el")        ; needs a rethink, or replacement
(jacob-load "jacob-init-amx.el")
