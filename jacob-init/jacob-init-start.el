;;; -*- lexical-binding: t -*-

(load "jacob-init-garbage-collection.el")
(load "jacob-init-package.el")
(load "jacob-init-use-package.el")      ; one day i should be able to remove this package from my config.
(load "jacob-init-environment-setup.el")
(load "jacob-init-gui-components.el")
(load "jacob-init-theme.el")
(load "jacob-init-modeline.el")
(load "jacob-init-fonts.el")

;; third party (why are these up here?)
(load "jacob-init-beacon.el")           ; another one to be removed. I want to replace this with the pulse.el library.
(load "jacob-init-dimmer.el")


;; * Built-in settings
;; ** don't ask for conformation when killing buffers with an attached process 
(use-package emacs
  :config
  (setq kill-buffer-query-functions (delq 'process-kill-buffer-query-function kill-buffer-query-functions))
  ;; this is for asking on exit
  (setq confirm-kill-processes nil))
;; ** recentf
(use-package recentf
  :commands recentf-open-files
  :config
  (recentf-mode 1))
;; ** keyboard macros
(use-package kmacro
  :defer 0.1
  :after xah-fly-keys
  :bind
  (:map xah-fly-r-keymap
        ("c" . kmacro-set-counter)))
;; ** dabbrev
(use-package dabbrev
  :config
  (setq dabbrev-case-fold-search nil)
  (setq dabbrev-case-replace nil)
  :bind (("M-/" . dabbrev-expand)
         ("C-M-/" . dabbrev-completion)))
;; ** put custom stuff in custom.el
(use-package cus-edit
  :defer 0.1
  :config
  (setq custom-file "~/.emacs.d/custom.el"))
;; ** prevent emacs from disabling commands.
(use-package novice
  :defer 0.1
  :config
  (setq disabled-command-function nil))
;; ** move by logical lines
(use-package simple
  :defer 0.1
  :config
  (setq line-move-visual t))
;; ** Use ls implemented in elisp to have consistent behaviour across platforms
(setq ls-lisp-use-insert-directory-program nil)
(setq ls-lisp-dirs-first t)
;; ** Read more data from process
(setq read-process-output-max (* 1024 1024))
;; ** Reduce lag
(setq auto-window-vscroll nil)
(setq redisplay-dont-pause t) ; obsolete
;; ** Stop the bell ringing all the time
(setq ring-bell-function 'ignore)
;; ** Prevent the view of the screen jumping to the middle when scrolling out of the view.
(setq scroll-conservatively 100)
;; ** subword-mode
(use-package subword
  :defer 0.1
  :config (global-subword-mode))

;; ** Make typing delete/overwrite region
(use-package delsel
  :defer 0.1
  :config
  (delete-selection-mode 1))

;; ** Turn on bracket match highlight
(use-package paren
  :defer 0.1
  :config
  (show-paren-mode 1))
;; ** Prevent emacs from creating debris when editing files
(use-package emacs
  :config
  (setq make-backup-files nil)
  (setq auto-save-default nil)
  (setq create-lockfiles nil))
;; ** Backups don't destroy original file's creation date
(setq backup-by-copying t)

;; ** Y/n instead of yes/no
(defalias 'yes-or-no-p 'y-or-n-p)

;; ** Expert mode for ibuffer
(setq ibuffer-expert t)
;; ** Add lisp folder to load path
(add-to-list 'load-path "~/.emacs.d/lisp/")
;; ** Save minibuffer history (useful for compile)
(use-package savehist
  :defer 0.1
  :config
  (setq savehist-file "~/.emacs.d/savehist")
  (setq history-length 1000)
  (setq history-delete-duplicates t)
  (setq savehist-save-minibuffer-history t))
;; ** Save cursor position in files
(use-package saveplace
  :config
  (setq save-place-file "~/.emacs.d/saveplace")
  (setq save-place-forget-unreadable-files t)
  (save-place-mode 1))
;; ** Startup time
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs ready in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))
;; ** Microsoft Windows functions
(if (string-equal system-type "windows-nt")
    (use-package emacs
      :config
      (setq w32-pass-rwindow-to-system nil)
      (setq w32-pass-apps-to-system nil)
      (setq w32-rwindow-modifier 'super)
      (setq w32-apps-modifier 'hyper)))

(load "jacob-init-tabs.el")
(load "jacob-init-personal-functions.el")

(load "jacob-init-xah-fly-keys.el")

(load "jacob-init-eglot.el")
(load "jacob-init-lsp-mode.el")         ; to be deprecated, use eglot only!!!

;; Major Mode Packages
;; all major mode packages need major review. need a writing style setup. activate necessary minor modes within appropriate major mode config, not in own minor mode config.

(load "jacob-init-php-mode.el")
(load "jacob-init-elisp-mode.el")
(load "jacob-init-bnf-mode.el")         ; probably uneeded
(load "jacob-init-org-mode.el")
(load "jacob-init-yaml-mode.el")        ; will come in
(load "jacob-init-c-mode.el")           ; needs major review (and is, also, probably uneeded outside of educational purposes.)
(load "jacob-init-java-mode.el")
(load "jacob-init-csharp-mode.el")      ; needs urgent review, this is my job language lolololol
(load "jacob-init-web-mode.el")         ; also important, as is used for editing razor templates
(load "jacob-init-json-mode.el")        ; will also come in
(load "jacob-init-ahk-mode.el")         ; almost certainly pointless

;; * Minor Mode Packages
;; ** flycheck
(use-package flycheck
  :ensure t
  :defer 2
  :config
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))
  (global-flycheck-mode))
;; ** which-key
(use-package which-key
  :ensure t
  :defer 2
  :config
  (which-key-mode))

;; ** company
(use-package company
  :ensure t
  :defer t
  :hook ((csharp-mode-hook java-mode-hook) . company-mode)
  :config
  (setq company-idle-delay 0.5)
  (setq company-minimum-prefix-length 3))

(use-package company-php
  :ensure t
  :after company
  :config
  (add-to-list (make-local-variable 'company-backends)
               '(company-ac-php-backend)))

;; ** projectile
(use-package projectile
  :ensure t
  :defer 2
  :config
  (projectile-mode t)
  (define-key xah-fly-dot-keymap (kbd "p") projectile-command-map)
  (setq projectile-completion-system 'ivy))
;; ** omnisharp
;; FIXME: if company mode is not started before csharp mode is entered, omnisharp mode will not activate
(use-package omnisharp
  :ensure t
  :defer t
  :bind
  (:map jacob-omnisharp-keymap
        ("u" . omnisharp-fix-usings)
        ("U" . omnisharp-find-usages)
        ("i" . omnisharp-find-implementations)
        ("d" . omnisharp-go-to-definition)
        ("r" . omnisharp-rename)
        ("a" . omnisharp-run-code-action-refactoring)
        ("o" . omnisharp-start-omnisharp-server)
        ("O" . omnisharp-stop-server))
  :config
  ;; at this point, company mode is enabled.
  (define-prefix-command 'jacob-omnisharp-keymap)
  (define-key xah-fly-dot-keymap (kbd "o") jacob-omnisharp-keymap)

  (setq omnisharp-company-ignore-case nil)
  (setq omnisharp-server-executable-path (expand-file-name jacob-omnisharp-file-path))

  (defun jacob-csharp-indent-or-complete ()
    (interactive)
    (if (region-active-p)
        (c-indent-line-or-region :region (region-bounds))
      (let ((old-point (point)))
        (c-indent-line-or-region)
        (if (eq old-point (point))
            (call-interactively 'counsel-company)))))

  (define-key csharp-mode-map (kbd "<tab>") 'jacob-csharp-indent-or-complete)
  :hook (csharp-mode-hook . omnisharp-mode))

;; *** add omnisharp to company backend
(use-package emacs
  :after company omnisharp
  :config
  (add-hook 'omnisharp-mode-hook (lambda ()
                                   (add-to-list (make-local-variable 'company-backends)
                                                '(company-omnisharp)))))
;; ** yasnippet
(use-package yasnippet
  :ensure t

  :hook
  (((eshell-mode-hook web-mode-hook python-mode-hook java-mode-hook csharp-mode-hook php-mode-hook) . yas-minor-mode))

  :config
  (yas-reload-all))

;; ** key-chord
(use-package key-chord
  :config
  (key-chord-mode 1))
;; ** outshine
(use-package outshine
  :ensure t
  :hook (emacs-lisp-mode-hook . outshine-mode))
;; ** olivetti
(use-package olivetti
  :ensure t
  :hook (org-mode-hook . olivetti-mode))
;; * Non-mode Packages
;; ** try
(use-package try
  :ensure t
  :defer 5)
;; ** avy
(use-package avy
  :ensure t
  :after key-chord
  :defer 0.1
  :config
  (setq avy-style 'at-full)
  (setq avy-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l ?\;))
  (setq avy-orders-alist '((avy-goto-end-of-line . avy-order-closest)
                           (avy-goto-word-or-subword-1 . avy-order-closest)))
  (setq avy-all-windows 'all-frames)
  (key-chord-define-global "fj" 'avy-goto-char)
  (key-chord-define-global "fk" 'avy-goto-end-of-line))
;; ** restart-emacs
(use-package restart-emacs
  :ensure t
  :defer t)

;; ** switch-window
(use-package switch-window
  :ensure t
  :defer t
  :config
  (setq switch-window-input-style 'minibuffer)
  (setq switch-window-threshold 2)
  (setq switch-window-multiple-frames t)
  (setq switch-window-shortcut-style 'qwerty)
  (setq switch-window-qwerty-shortcuts
        '("q" "w" "e" "r" "a" "s" "d" "f" "z" "x" "c" "v"))
  :bind
  ([remap xah-next-window-or-frame] . switch-window))

(load "jacob-init-ivy.el")              ; should replace
(load "jacob-init-expand-region.el")
(load "jacob-init-shell-pop.el")        ; needs a rethink, or replacement

;; ** amx
(use-package amx
  :ensure t
  :defer 1
  :config
  (amx-mode 1))
