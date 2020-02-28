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

(setq w32-pass-rwindow-to-system nil
	  w32-rwindow-modifier 'super)

(setq w32-pass-apps-to-system nil)
(setq w32-apps-modifier 'hyper)

;; use spaces to indent
(progn
  (setq-default indent-tabs-mode nil))

;; set default tab char's display width to 4 spaces
(setq-default tab-width 4)

;; (defun my-insert-tab-char ()
  ;; "Insert a tab char. (ASCII 9, \t)"
  ;; (interactive)
  ;; (insert "\t"))

;; (global-set-key (kbd "TAB") 'my-insert-tab-char) ; same as Ctrl+i

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

  (defun jacob-turn-off-xah ()
    (interactive)
    (xah-fly-insert-mode-activate))

  (add-hook 'dired-mode-hook 'jacob-turn-off-xah)
  (add-hook 'eww-mode-hook 'jacob-turn-off-xah)
  (add-hook 'dashboard-mode-hook 'jacob-turn-off-xah)
  (add-hook 'ibuffer-mode-hook 'jacob-turn-off-xah)
  (add-hook 'custom-mode-hook 'jacob-turn-off-xah)

  :bind (:map jacob-config-keymap
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

(defun my-csharp-mode-setup ()
  (setq c-syntactic-indentation t)
  (c-set-style "ellemtel")
  (setq c-basic-offset 4))

(add-hook 'csharp-mode-hook 'my-csharp-mode-setup t)

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

  :init
  (define-prefix-command 'jacob-avy-keymap)

  :bind
  (:map xah-fly-dot-keymap
        ("a" . jacob-avy-keymap)
        :map jacob-avy-keymap
        ("a" . avy-goto-char)
        ("s" . avy-goto-word-1)
        ("d" . avy-goto-line)
        ("f" . avy-goto-end-of-line)))

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
  (key-chord-mode 1)
  (key-chord-define xah-fly-key-map "fd" 'xah-fly-command-mode-activate-no-hook))

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

(use-package restart-emacs
  :ensure t
  :defer t)

(use-package smex
  :ensure t
  :config (smex-initialize)
  :bind
  ("M-x" . smex))

(use-package spaceline
  :ensure t
  :config
  (setq powerline-default-seperator (quote arrow))
  :init
  (spaceline-spacemacs-theme)
  )

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

(use-package dashboard
  :ensure t
  :config

  (defun jacob-go-to-dashboard ()
    (interactive)
    (switch-to-buffer "*dashboard*"))

  (define-key 'xah-fly-dot-keymap (kbd "d") 'jacob-go-to-dashboard)

  (dashboard-setup-startup-hook)
  (setq dashboard-items '((recents . 10)
                          (projects . 5)))
  (setq dashboard-set-footer nil)
  (setq dashboard-banner-logo-title "千里之行，始於足下")
  (setq dashboard-center-content t))

(use-package popup-kill-ring
  :ensure t
  :bind
  (:map xah-fly-dot-keymap ("v" . popup-kill-ring)))

(use-package swiper
  :ensure t
  :bind (
         :map xah-fly-dot-keymap
              ("s" . swiper)))

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
  :bind
  (:map xah-fly-dot-keymap
		("=" . 'er/expand-region)))

(use-package shell-pop
  :ensure t
  :bind
  (:map xah-fly-n-keymap
		("d" . shell-pop)))

(use-package move-text
  :ensure t
  :defer t
  :config
  (move-text-default-bindings))

(use-package eshell-up
  :ensure t
  :defer t)

(use-package langtool
  ;; :ensure t
  :defer t
  :config
  (setq langtool-language-tool-jar
		"/home/lem/Documents/LanguageTool-4.8/languagetool-commandline.jar"))
