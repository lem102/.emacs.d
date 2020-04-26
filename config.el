(setq-default mode-line-format
              (list
               ;; saved, readonly
               "%*"
               ;; major mode
               "%m: "
               ;; buffer name
               "%b "
               ;; position of point
               "(%c,%l) "
               ;; time
               '(:eval (format-time-string "%H:%M" (current-time)))))

(setq ido-enable-flex-matching t)
(setq ido-create-new-buffer 'always)
(setq ido-everywhere t)
(ido-mode 1)

(when window-system (global-hl-line-mode t))

(setq ring-bell-function 'ignore)

(setq scroll-conservatively 100)

(tool-bar-mode -1)

(menu-bar-mode -1)

(scroll-bar-mode -1)

(electric-pair-mode 1)

(global-subword-mode 1)

(delete-selection-mode 1)

(show-paren-mode 1)

(setq make-backup-files nil)

(setq auto-save-default nil)

(setq create-lockfiles nil)

(setq backyp-by-copying t)

(defalias 'yes-or-no-p 'y-or-n-p)

(setq display-time-24hr-format t)
(display-time-mode 1)

(setq ibuffer-expert t)

(require 'dired)
(define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file)
(define-key dired-mode-map (kbd "^")(lambda () (interactive)(find-alternate-file "..")))

(setq dired-dwim-target t)

(defun xah-dired-mode-setup()
      (dired-hide-details-mode 1))
(add-hook 'dired-mode-hook 'xah-dired-mode-setup)

(toggle-truncate-lines)

(add-to-list 'load-path "~/.emacs.d/lisp/")

(when (member "DejaVu Sans Mono" (font-family-list))
	(add-to-list 'initial-frame-alist '(font . "DejaVu Sans Mono-10"))
	(add-to-list 'default-frame-alist '(font . "DejaVu Sans Mono-10")))

(desktop-save-mode 1)

(savehist-mode 1)

(setq w32-pass-rwindow-to-system nil
	      w32-rwindow-modifier 'super)

(setq w32-pass-apps-to-system nil)
(setq w32-apps-modifier 'hyper)

;; use spaces to indent
(setq-default indent-tabs-mode nil)
;; set default tab char's display width to 4 spaces
(setq-default tab-width 4)
;; make tab key call indent command or insert tab character, depending on cursor position
(setq-default tab-always-indent nil)

(use-package xah-fly-keys

  :ensure t

  :demand

  :custom
  (xah-fly-use-control-key nil)

  :config 
  (defun config-visit ()
    (interactive)
    (find-file "~/.emacs.d/config.org"))

  (defun config-reload ()
    (interactive)
    (org-babel-load-file (expand-file-name "~/.emacs.d/config.org")))

  (defun jacob-org-src-block ()
    (interactive)
    (if (bound-and-true-p org-src-mode)
        (org-edit-src-exit)
      (if (equal major-mode 'org-mode)
          (org-edit-special))))

  (defun jacob-recompile-packages ()
    (interactive)
    (byte-recompile-directory package-user-dir nil 'force))

  (define-prefix-command 'jacob-config-keymap)
  (xah-fly-keys-set-layout "qwerty")
  (xah-fly-keys 1)

  (defun jacob-xah-command-binds ()
    (interactive)
    (define-key xah-fly-key-map (kbd "a") 'counsel-M-x)
    (define-key xah-fly-key-map (kbd "n") 'swiper)
    (define-key xah-fly-key-map (kbd "8") 'er/expand-region))

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
        ("p" . jacob-recompile-packages))
  (:map xah-fly-dot-keymap
        ("c" . jacob-config-keymap)))

;; (use-package sunrise
  ;; :bind
  ;; (:map xah-fly-leader-key-map
        ;; ("m" . sunrise)))

(add-to-list 'org-structure-template-alist
			     '("el" "#+BEGIN_SRC emacs-lisp\n?\n#+END_SRC"))

(use-package yaml-mode
      :ensure t
      :config (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode)))

(setq-default c-basic-offset 4)

(use-package csharp-mode
  :ensure t
  :config
  (defun my-csharp-mode-setup ()
    (setq c-syntactic-indentation t)
    (c-set-style "ellemtel")
    (setq c-basic-offset 4))
  :hook
  (csharp-mode . my-csharp-mode-setup)
  :mode
  ("\\.cs\\$" . csharp-mode))

(setq load-path (append (list (expand-file-name "~/.emacs.d/LilyPond/")) load-path))

(autoload 'LilyPond-mode "lilypond-mode" "LilyPond Editing Mode" t)
(add-to-list 'auto-mode-alist '("\\.ly$" . LilyPond-mode))
(add-to-list 'auto-mode-alist '("\\.ily$" . LilyPond-mode))
(add-hook 'LilyPond-mode-hook (lambda () (turn-on-font-lock)))

(use-package web-mode
  :ensure t

  :preface (defun jacob-web-mode-config ()
             (interactive)
             (setq-local electric-pair-pairs '((?\" . ?\") (?\< . ?\>)))
             (yas-activate-extra-mode 'html-mode))

  :config (setq web-mode-engines-alist
                '(("razor"	. "\\.cshtml\\'")))

  :hook (web-mode . jacob-web-mode-config)

  :custom (web-mode-markup-indent-offset 2)
  (web-mode-css-indent-offset 2)
  (web-mode-code-indent-offset 2)

  :mode (("\\.html?\\'" . web-mode)
         ("\\.cshtml\\'" . web-mode)
         ("\\.css\\'" . web-mode)))

(use-package js2-mode
  :ensure t
  :mode ("\\.js\\'" . js2-mode)
  :hook (js2-mode . js2-imenu-extras-mode))

(use-package js2-refactor
  :ensure t
  :hook (js2-mode . js2-refactor-mode)
  :config (js2r-add-keybindings-with-prefix "C-c C-r"))

(use-package json-mode
  :ensure t
  :mode ("\\.json\\$" . json-mode))

(use-package clojure-mode
  :ensure t)

(use-package beacon
      :ensure t
      :diminish
      :config
      (beacon-mode 1))

(use-package which-key
      :ensure t
      :diminish
      :config
      (which-key-mode))

(use-package ido-vertical-mode
      :ensure t
      :config
      (ido-vertical-mode 1))

(use-package company
      :ensure t
      :diminish
      :config
      (setq company-idle-delay 0.5)
      (setq company-minimum-prefix-length 3)
      (global-company-mode t)
      (add-hook 'eshell-mode-hook (lambda () (company-mode -1))))

(use-package projectile
  :ensure t
  :diminish
  :bind
  (:map xah-fly-dot-keymap
        ("p" . projectile-command-map))
  :config
  (projectile-mode t))

(use-package avy
  :ensure t

  :config
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
      :config
      (dimmer-mode))

(use-package highlight-parentheses
      :ensure t
      :diminish
      :init
      (define-globalized-minor-mode global-highlight-parentheses-mod
	highlight-parentheses-mode
	(lambda ()
	      (highlight-parentheses-mode t)))
      (global-highlight-parentheses-mode t))

(use-package omnisharp
   :ensure t
   :hook (csharp-mode . omnisharp-mode)
   :init (define-prefix-command 'jacob-omnisharp-keymap)
   :bind
   (:map xah-fly-dot-keymap
         ("o" . jacob-omnisharp-keymap)
         :map jacob-omnisharp-keymap
         ("u" . omnisharp-fix-usings)
         ("d" . omnisharp-go-to-definition)
         ("s" . omnisharp-start-omnisharp-server)
         ("S" . omnisharp-stop-server))
   :config
   (add-hook 'omnisharp-mode-hook (lambda ()
                                    (add-to-list (make-local-variable 'company-backends)
                                                 '(company-omnisharp))))
   (setq omnisharp-server-executable-path "~\\..\\omnisharp-win-x86\\OmniSharp.exe"))

(use-package yasnippet
  :ensure t
  :hook ((csharp-mode . yas-minor-mode)
         (web-mode . yas-minor-mode)))

(use-package yasnippet-snippets
      :ensure t)

(use-package key-chord
  :config
  (key-chord-mode 1))

(use-package elpy
  :ensure t
  :init
  (elpy-enable))

(use-package flycheck
      :ensure t
      :init
      (global-flycheck-mode t)
      ;; For some reason, I am unable to diminish flycheck with :diminish
      (diminish 'flycheck-mode)
      :config
      (when (require 'flycheck nil t)
	(setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
	(add-hook 'elpy-mode-hook 'flycheck-mode)))

(use-package slime
  :ensure t
  :config
  (setq inferior-lisp-program "sbcl")
  (setq slime-cobtribs '(slime-fancy))
  :bind
  (:map slime-mode-map
        ("SPC" . nil)))

(use-package cider
  :diminish
  :ensure t)

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

(use-package popup-kill-ring
      :ensure t
      :bind
      (:map xah-fly-dot-keymap ("v" . popup-kill-ring)))

(use-package ivy
  :ensure t
  :diminish
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t))

(use-package counsel
  :ensure t
  :diminish
  :init (counsel-mode 1))

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
  :custom
  (expand-region-contract-fast-key "9"))

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
