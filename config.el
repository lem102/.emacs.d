(setq read-process-output-max (* 1024 1024))

;; (setq inhibit-startup-message t)

(setq auto-window-vscroll nil)
(setq redisplay-dont-pause t)

(setq ring-bell-function 'ignore)

(setq scroll-conservatively 100)

(electric-pair-mode 1)

(global-subword-mode 1)

(delete-selection-mode 1)

(show-paren-mode 1)

(setq make-backup-files nil)

(setq auto-save-default nil)

(setq create-lockfiles nil)

(setq backup-by-copying t)

(defalias 'yes-or-no-p 'y-or-n-p)

(setq ibuffer-expert t)

(toggle-truncate-lines)

(add-to-list 'load-path "~/.emacs.d/lisp/")

(savehist-mode 1)

(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs ready in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

(setq w32-pass-rwindow-to-system nil
	  w32-rwindow-modifier 'super)

(setq w32-pass-apps-to-system nil)
(setq w32-apps-modifier 'hyper)

;; use spaces to indent
(setq-default indent-tabs-mode nil)
;; set default tab char's display width to 4 spaces
(setq-default tab-width 4)
;; make tab key call indent command or insert tab character, depending on cursor position
(setq-default tab-always-indent t)

(defun jacob-quit-popup-window ()
  (interactive)
  (let ((loop-list (window-list))
        (window-not-found t))
    (while (and loop-list window-not-found)
      (let* ((window (car loop-list))
             (mode (jacob-buffer-mode (window-buffer window))))
        (if (or (eq mode 'help-mode)
                (eq mode 'compilation-mode)
                (eq mode 'special-mode))
            (progn
              (quit-window :window window)
              (setq window-found nil))))
      (setq loop-list (cdr loop-list)))))

(defun jacob-buffer-mode (buffer-or-string)
  "Returns the major mode associated with a buffer."
  (with-current-buffer buffer-or-string
     major-mode))

(defun config-visit ()
  (interactive)
  (find-file "~/.emacs.d/config.org"))

(defun config-reload ()
  (interactive)
  (org-babel-load-file (expand-file-name "~/.emacs.d/config.org")))

(defun jacob-org-src-block ()
  "Replacement for C-c ' in both \"org-mode\" and when editing code blocks within \"org-mode\"."
  (interactive)
  (if (bound-and-true-p org-src-mode)
      (org-edit-src-exit)
    (if (equal major-mode 'org-mode)
        (org-edit-special))))

(defun jacob-recompile-packages ()
  "Recompile all packages."
  (interactive)
  (byte-recompile-directory package-user-dir nil 'force))

(defun jacob-split-window-below-select-new ()
  "Splits current window vertically, then switch to new window."
  (interactive)
  (split-window-below)
  (other-window 1))


(defun jacob-split-window-right-select-new ()
  "Splits current window horizontally, then switch to new window."
  (interactive)
  (split-window-right)
  (other-window 1))

(use-package xah-fly-keys
  :ensure t

  :demand

  :init
  (setq xah-fly-use-control-key nil)

  (defun jacob-xah-command-binds ()
    "Set custom keys for xah-fly-keys keybindings."
    (define-key xah-fly-key-map (kbd "a") 'counsel-M-x)
    (define-key xah-fly-key-map (kbd "n") 'swiper)
    (define-key xah-fly-key-map (kbd "8") 'er/expand-region)
    (define-key xah-fly-key-map (kbd "4") 'jacob-split-window-below-select-new)
    (define-key xah-fly-key-map (kbd "2") 'jacob-quit-popup-window)) ;; 1 can be rebound, is bound to a inferior version of expand region

  :config
  (define-prefix-command 'jacob-config-keymap)
  (xah-fly-keys-set-layout "qwerty")
  (xah-fly-keys 1)

  (add-hook 'xah-fly-command-mode-activate-hook 'jacob-xah-command-binds)
  (jacob-xah-command-binds) ;; call it on startup so binds are set without calling xah-fly-command-mode-activate first.

  (add-hook 'dired-mode-hook 'xah-fly-keys-off)
  (add-hook 'eww-mode-hook 'xah-fly-keys-off)
  (add-hook 'ibuffer-mode-hook 'xah-fly-keys-off)
  (add-hook 'custom-mode-hook 'xah-fly-keys-off)

  (key-chord-define xah-fly-key-map "fd" 'xah-fly-command-mode-activate)

  :bind
  (:map jacob-config-keymap
        ("r" . config-reload)
        ("R" . restart-emacs)
        ("e" . config-visit)
        ("c" . jacob-org-src-block)
        ("p" . jacob-recompile-packages)
        ("t" . jacob-long-time-toggle))
  (:map xah-fly-dot-keymap
        ("c" . jacob-config-keymap))
  (:map xah-fly-leader-key-map
        ("4" . jacob-split-window-right-select-new)))

(use-package lsp-mode
  :ensure t
  :hook
  (java-mode . lsp)
  (lsp-mode . lsp-enable-which-key-integration)
  :commands lsp
  :init
  (setq lsp-completion-enable-additional-text-edit nil)
  (setq lsp-prefer-capf t)
  (setq lsp-prefer-flymake nil)
  :config
  (define-key xah-fly-dot-keymap (kbd "l") lsp-command-map))

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode)

(use-package lsp-ivy
  :ensure t
  :commands lsp-ivy-workspace-symbol)

(use-package dap-mode
  :ensure t
  :hook java-mode
  :config
  (use-package dap-java)
  (dap-mode 1)
  (dap-ui-mode 1)
  (dap-tooltip-mode 1)
  (tooltip-mode 1)
  (dap-ui-controls-mode 1))

(use-package lsp-java
  :ensure t)

(use-package dired
  :config
  (defun jacob-teardown-xah-for-wdired ()
    (interactive)
    (wdired-finish-edit)
    (define-key xah-fly-leader-key-map (kbd ";") 'save-buffer)
    (xah-fly-keys-off))

  (defun jacob-setup-xah-for-wdired ()
    (interactive)
    (xah-fly-keys)
    (define-key xah-fly-leader-key-map (kbd ";") 'jacob-teardown-xah-for-wdired))

  (add-hook 'wdired-mode-hook 'jacob-setup-xah-for-wdired)

  (define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file)
  (define-key dired-mode-map (kbd "^")(lambda () (interactive)(find-alternate-file "..")))
  (setq dired-dwim-target t)

  :bind
  (:map dired-mode-map
        ("," . switch-window)
        ("SPC" . xah-fly-leader-key-map)
        ("p" . dired-maybe-insert-subdir)
        ("i" . dired-previous-line)
        ("k" . dired-next-line)
        ("n" . swiper)
        ("f" . dired-toggle-read-only)
        ("q" . xah-close-current-buffer)))

(use-package org
  :mode ("\\.org\\'" . org-mode)
  :config
  (add-to-list 'org-structure-template-alist
             '("el" "#+BEGIN_SRC emacs-lisp\n?\n#+END_SRC")))

(use-package yaml-mode
  :ensure t
  :defer t
  :mode ("\\.yml\\'" . yaml-mode))

(setq-default c-basic-offset 4)

(use-package csharp-mode
  :ensure t
  :defer t
  :config
  (defun my-csharp-mode-setup ()
    (setq c-syntactic-indentation t)
    (c-set-style "ellemtel")
    (setq c-basic-offset 4))
  :hook
  (csharp-mode . my-csharp-mode-setup)
  :mode
  ("\\.cs\\$" . csharp-mode))

(use-package web-mode
  :ensure t

  :preface
  (defun jacob-web-mode-config ()
    (interactive)
    (setq-local electric-pair-pairs '((?\" . ?\") (?\< . ?\>)))
    (yas-activate-extra-mode 'html-mode))

  :config
  (setq web-mode-engines-alist
                '(("razor"	. "\\.cshtml\\'")))
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)

  :hook (web-mode . jacob-web-mode-config)

  :mode (("\\.html?\\'" . web-mode)
         ("\\.cshtml\\'" . web-mode)
         ("\\.css\\'" . web-mode)))

(use-package json-mode
  :ensure t
  :mode ("\\.json\\$" . json-mode))

(use-package clojure-mode
  :ensure t
  :mode ("\\.clj\\$" . clojure-mode))

(use-package gdscript-mode
  :ensure t

  :config 
  (setq gdscript-use-tab-indents nil))

(use-package flycheck
  :ensure t
  :defer 2
  :config
  (global-flycheck-mode))

(use-package beacon
  :ensure t
  :defer 2
  :diminish
  :config
  (beacon-mode 1))

(use-package which-key
  :ensure t
  :defer 2
  :diminish
  :config
  (which-key-mode))

(use-package company
  :ensure t
  :defer t
  :diminish
  :hook ((emacs-lisp-mode csharp-mode java-mode) . company-mode)
  :config
  (setq company-idle-delay 0.5)
  (setq company-minimum-prefix-length 3))

(use-package projectile
  :ensure t
  :defer 2
  :diminish
  :config
  (projectile-mode t)
  (define-key xah-fly-dot-keymap (kbd "p") projectile-command-map)
  (setq projectile-completion-system 'ivy))

(use-package avy
  :ensure t
  :defer 1
  :config
  (setq avy-keys (number-sequence ?a ?z))
  (setq avy-all-windows t)
  (setq avy-orders-alist
        '((avy-goto-char-timer . avy-order-closest)
          (avy-goto-end-of-line . avy-order-closest)))
  (key-chord-define xah-fly-key-map "fj" 'avy-goto-char-timer)
  (key-chord-define xah-fly-key-map "fk" 'avy-goto-word-or-subword-1)
  (key-chord-define xah-fly-key-map "fl" 'avy-goto-line)
  (key-chord-define xah-fly-key-map "f;" 'avy-goto-end-of-line))

(use-package rainbow-mode
  :ensure t
  :diminish
  :hook prog-mode)

(use-package dimmer
  :ensure t
  :defer 5
  :config
  (dimmer-mode))

(use-package omnisharp
   :ensure t
   :defer t
   :after company
   :hook (csharp-mode . omnisharp-mode)
   :bind
   (:map jacob-omnisharp-keymap
         ("u" . omnisharp-fix-usings)
         ("d" . omnisharp-go-to-definition)
         ("s" . omnisharp-start-omnisharp-server)
         ("S" . omnisharp-stop-server))
   :config
   (define-prefix-command 'jacob-omnisharp-keymap)
   (define-key xah-fly-dot-keymap (kbd "o") jacob-omnisharp-keymap)
   (add-hook 'omnisharp-mode-hook (lambda ()
                                    (add-to-list (make-local-variable 'company-backends)
                                                 '(company-omnisharp))))
   (setq omnisharp-company-ignore-case nil)
   (setq omnisharp-server-executable-path "D:\\Programming\\OmniSharp\\omnisharp-roslyn\\bin\\Debug\\OmniSharp.Stdio.Driver\\net472\\OmniSharp.exe"))

(use-package yasnippet
  :ensure t

  :hook
  (((csharp-mode web-mode) . yas-minor-mode))

  :config
  (yas-reload-all))

(use-package key-chord
  :defer 1

  :config
  (key-chord-mode 1))

(use-package cider
  :diminish
  :ensure t
  :mode ("\\.clj\\$" . clojure-mode))

(use-package restart-emacs
  :ensure t
  :defer t)

(use-package smex
  :ensure t
  :config (smex-initialize)
  :bind
  ("M-x" . smex))

(use-package diminish
  :ensure t
  :defer t
  :config
  (diminish 'subword-mode)
  (diminish 'org-src-mode)
  (diminish 'eldoc-mode))

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

(use-package ivy
  :ensure t
  :diminish
  :defer 1

  :bind
  (:map xah-fly-leader-key-map
        ("v" . counsel-yank-pop))

  :config
  (ivy-mode 1)
  (setq ivy-initial-inputs-alist nil)
  (setq enable-recursive-minibuffers t))

(use-package swiper
  :ensure t
  :after ivy)

(use-package counsel
  :ensure t
  :diminish
  :after ivy

  :config (counsel-mode))

(use-package multiple-cursors
  :ensure t
  :bind
  (:map xah-fly-dot-keymap
		("m" . jacob-multiple-cursors-keymap)
  :map jacob-multiple-cursors-keymap
		("l" . mc/edit-lines)
		(">" . mc/mark-next-like-this)
		("<" . mc/mark-previous-like-this)
		("a" . mc/mark-all-like-this))
  :init
  (define-prefix-command 'jacob-multiple-cursors-keymap))

(use-package expand-region
  :ensure t

  :config
  (setq expand-region-contract-fast-key "9"))

(use-package shell-pop
  :ensure t
  :init
  (defun jacob-shell-pop-eshell ()
  (interactive)
  (let ((shell-pop-shell-type '("eshell" "*eshell*" (lambda () (eshell))))
        (shell-pop-term-shell "eshell"))
    (shell-pop--set-shell-type 'shell-pop-shell-type shell-pop-shell-type)
    (call-interactively 'shell-pop)))

  (defun jacob-shell-pop-shell ()
    (interactive)
    (let ((shell-file-name "/bin/bash")
          (shell-pop-shell-type '("shell" "*shell*" (lambda () (shell))))
          (shell-pop-term-shell "shell"))
      (shell-pop--set-shell-type 'shell-pop-shell-type shell-pop-shell-type)
      (call-interactively 'shell-pop)))
  :bind
  (:map xah-fly-n-keymap
        ("d" . jacob-shell-pop-eshell)
        ("f" . jacob-shell-pop-shell)))

(use-package move-text
  :ensure t
  :config
  (move-text-default-bindings))

(use-package eshell-up
  :ensure t)

(use-package langtool
  ;; :ensure t
  :defer t
  :config
  (setq langtool-language-tool-jar
		"/home/lem/Documents/LanguageTool-4.8/languagetool-commandline.jar"))

(use-package color-theme-sanityinc-tomorrow
  :ensure t)

(load "~/.emacs.d/myLisp/jacob-long-time")
(jacob-long-time-toggle)

(use-package hl-line
  :defer 2
  :config (when window-system (global-hl-line-mode t)))

(tool-bar-mode -1)

(menu-bar-mode -1)

(scroll-bar-mode -1)

(when (member "DejaVu Sans Mono" (font-family-list))
	(add-to-list 'initial-frame-alist '(font . "DejaVu Sans Mono-10"))
	(add-to-list 'default-frame-alist '(font . "DejaVu Sans Mono-10")))
