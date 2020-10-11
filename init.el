(defmacro measure-time (&rest body)
  `(let ((time (current-time)))
     ,@body
     (message "%.06f" (float-time (time-since time)))))

(require 'package)

(setq package-enable-at-startup nil)
(add-to-list 'package-archives
			 '("melpa" . "https://melpa.org/packages/"))
(unless package--initialized (package-initialize))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; go to the nice org file :)
;; (require 'org)
;; (org-babel-load-file (expand-file-name "~/.emacs.d/config.org"))

;; * Use-Package
;; Make sure use package is available.
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Configure use-package prior to loading it.
(eval-and-compile
  (setq use-package-always-ensure nil)
  (setq use-package-always-defer nil)
  (setq use-package-always-demand nil)
  (setq use-package-enable-imenu-support t)
  ;; use real name of hook instead of shorter version.
  (setq use-package-hook-name-suffix nil))

(eval-when-compile
  (require 'use-package))

;; * Set up variables
(use-package emacs
  :config
  (load-file (expand-file-name "~/.emacs.d/config-local-settings.el")))
;; * Appearance
;; ** Disable GUI components
(use-package emacs
  
  :config
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (scroll-bar-mode -1)

  (setq use-file-dialog nil)
  (setq use-dialog-box t)
  (setq inhibit-startup-message t)
  :bind
  (("C-z" . nil)
   ("C-c C-z" . nil)
   ("C-h h" . nil)))
;; ** Theme
;; *** manoj-dark
(use-package emacs
  :disabled
  :config
  (load-theme 'manoj-dark t))
;; *** sanityinc-tomorrow-blue
(use-package color-theme-sanityinc-tomorrow
  :ensure t
  :defer 0.1
  :config
  (load-theme 'sanityinc-tomorrow-blue t))
;; ** Modeline
(use-package emacs
  :config
  (setq-default mode-line-format (list
                                  ;; saved, readonly
                                  "%*"
                                  ;; major mode
                                  "%m: "
                                  ;; buffer name
                                  "%b "
                                  ;; position of point
                                  "(%l,%c) ")))
;; ** Fonts
(use-package frame
  :config
  (let* ((default-font-size (font-get (face-attribute 'default :font) :height))
         (default-font-family (font-get (face-attribute 'default :font) :family))
         (font-name (concat (if jacob-font-family
                                (format "%s" jacob-font-family)
                              (format "%s" default-font-family))
                            "-"
                            (if jacob-font-size
                                (format "%s" jacob-font-size)
                              (format "%s" default-font-size)))))
    (add-to-list 'default-frame-alist `(font . ,font-name))))
;; ** beacon
(use-package beacon
  :ensure t
  :demand
  :config
  (beacon-mode 1))
;; ** dimmer
(use-package dimmer
  :ensure t
  :defer 5
  :config
  (dimmer-mode))
;; ** default frame size on startup
(use-package emacs
  :config
  (if (display-graphic-p)
      (setq initial-frame-alist
            '(
              (tool-bar-lines . 0)
              (width . 116)
              (fullscreen . fullheight)
              (left . 0)
              (top . 0)))))
;; * Built-in settings
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
(setq redisplay-dont-pause t)
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
;; ** Tabs
(use-package emacs
  :config
  ;; use spaces to indent
  (setq-default indent-tabs-mode nil)
  ;; set default tab char's display width to 4 spaces
  (setq-default tab-width 4)
  ;; make tab key call indent command or insert tab character, depending on cursor position
  (setq-default tab-always-indent 'complete))
;; * Personal Functions
(defun jacob-original-find-file ()
  "Uses the original file-file mechanism. 
  Useful for dealing with files on other servers.
  (at least on Microsoft Windows)"
  (interactive)
  (let ((completing-read-function 'completing-read-default))
    (call-interactively 'find-file)))

(defun eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))

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

(defun jacob-config-visit ()
  (interactive)
  (find-file "~/.emacs.d/init.el"))

(defun jacob-config-reload ()
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

(load-file (expand-file-name "~/.emacs.d/myLisp/jacob-long-time.el"))

(defun jacob-display-time ()
  "Display the current date and time in the echo area."
  (interactive)
  (message (concat (format-time-string "%A the %e")
                   (jacob-day-suffix (string-to-number (format-time-string "%e")))
                   (format-time-string " of %B, the year of our Lord %Y, ")
                   "at "
                   (jacob-long-time (string-to-number (format-time-string "%H"))
                                    (string-to-number (format-time-string "%M")))
                   ".")))

;; * Xah Fly Keys
(use-package xah-fly-keys
  :ensure t

  :demand

  :init
  (setq xah-fly-use-control-key nil)

  ;; This is a keyboard macro that enters insert mode, presses a backspace, then returns to command mode.
  ;; It's purpose is so I can bind "D" in command mode to whatever backspace does in any given buffer.
  (fset 'backspace
        [?f backspace home])

  (fset 'enter
        [return])

  (defun jacob-xah-command-binds ()
    "Set custom keys for xah-fly-keys keybindings."
    (interactive)
    (define-key xah-fly-key-map (kbd "a") 'counsel-M-x)
    (define-key xah-fly-key-map (kbd "s") 'enter)
    (define-key xah-fly-key-map (kbd "8") 'er/expand-region)
    (define-key xah-fly-key-map (kbd "4") 'jacob-split-window-below-select-new)
    ;; 1 can be rebound, is bound to a inferior version of expand region
    (define-key xah-fly-key-map (kbd "2") 'jacob-quit-popup-window))

  :config
  (load-file (expand-file-name "~/.emacs.d/myLisp/jacob-xah-modified-commands.el"))

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
        ("r" . jacob-config-reload)
        ("R" . restart-emacs)
        ("e" . jacob-config-visit)
        ("c" . jacob-org-src-block)
        ("p" . jacob-recompile-packages)
        ("t" . jacob-display-time))
  (:map xah-fly-e-keymap
        ("k". jacob-xah-insert-paren)
        ("l". jacob-xah-insert-square-bracket)
        ("j". jacob-xah-insert-brace)
        ("u". jacob-xah-insert-ascii-double-quote)
        ("i". jacob-xah-insert-ascii-single-quote)
        ("m" . xah-insert-hyphen)
        ("," . xah-insert-low-line)
        ("." . jacob-insert-equals)
        ("/" . jacob-insert-plus)
        ("z" . jacob-insert-apostrophe)
        ("x" . jacob-insert-at)
        ("c" . jacob-insert-hash)
        ("d" . backspace)
        ("v" . jacob-insert-tilde))
  (:map xah-fly-dot-keymap
        ("c" . jacob-config-keymap))
  (:map xah-fly-leader-key-map
        ("4" . jacob-split-window-right-select-new))
  (:map xah-fly-w-keymap
        ("n" . eval-and-replace)))
;; * Language Server Protocol & Debug Adapter Protocol
;; Language Server Protocol is an excellent way to get autocompletion, documentation
;; and linting for many programming languages within emacs. Therefore this
;; will eventually be quite a busy section.
;; ** Base lsp-mode
;; *** lsp-mode
(use-package lsp-mode
  :ensure t
  :hook
  ((java-mode-hook python-mode-hook php-mode-hook) . lsp)
  (lsp-mode-hook . lsp-enable-which-key-integration)
  :commands lsp
  :init
  (setq lsp-completion-enable-additional-text-edit nil)
  (setq lsp-prefer-capf nil)
  (setq lsp-prefer-flymake nil)
  :config
  (define-key xah-fly-dot-keymap (kbd "l") lsp-command-map))

;; *** lsp-ui
(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode)

;; *** lsp-ivy
(use-package lsp-ivy
  :ensure t
  :commands lsp-ivy-workspace-symbol)
;; ** Base dsp-mode
(use-package dap-mode
  :ensure t
  :hook java-mode-hook
  :config
  (use-package dap-java)
  (dap-mode 1)
  (dap-ui-mode 1)
  (dap-tooltip-mode 1)
  (tooltip-mode 1)
  (dap-ui-controls-mode 1))

;; ** Language Specific
;; *** lsp-java
(use-package lsp-java
  :ensure t
  :hook java-mode)
;; *** lsp-python-ms
(use-package lsp-python-ms
  :ensure t
  :disabled
  :init (setq lsp-python-ms-auto-install-server t))
;; * Dired
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
        ("n" . isearch-forward)
        ("f" . dired-toggle-read-only)
        ("q" . xah-close-current-buffer)))
;; * Major Mode Packages
;; ** php-mode
(use-package php-mode
  :ensure t
  :mode (("\\.php\\'" . php-mode)))
;; ** emacs-lisp-mode
(use-package elisp-mode
  :config
  (defun contrib/completing-read-in-region (start end collection &optional predicate)
    "Prompt for completion of region in the minibuffer if non-unique.
    Use as a value for `completion-in-region-function'."
    (if (and (minibufferp) (not (string= (minibuffer-prompt) "Eval: ")))
        (completion--in-region start end collection predicate)
      (let* ((initial (buffer-substring-no-properties start end))
             (limit (car (completion-boundaries initial collection predicate "")))
             (all (completion-all-completions initial collection predicate
                                              (length initial)))
             (completion (cond
                          ((atom all) nil)
                          ((and (consp all) (atom (cdr all)))
                           (concat (substring initial 0 limit) (car all)))
                          (t (completing-read
                              "Completion: " collection predicate t initial)))))
        (if (null completion)
            (progn (message "No completion") nil)
          (delete-region start end)
          (insert completion)
          t))))

  (setq completion-in-region-function #'contrib/completing-read-in-region))
;; ** bnf-mode
(use-package bnf-mode
  :ensure t)
;; ** Org
(use-package org
  :mode ("\\.org\\'" . org-mode)
  :config
  (setq-default yas-indent-line 'fixed)
  (add-to-list 'org-structure-template-alist
               '("el" "#+BEGIN_SRC emacs-lisp\n?\n#+END_SRC")))

;; ** yaml-Mode
(use-package yaml-mode
  :ensure t
  :defer t
  :mode ("\\.yml\\'" . yaml-mode))

;; ** c-mode
;; *** tab width
(setq-default c-basic-offset 4)

;; ** csharp-mode
(use-package csharp-mode
  :ensure t
  :config
  (defun my-csharp-mode-setup ()
    (setq c-syntactic-indentation t)
    (c-set-style "ellemtel")
    (setq c-basic-offset 4)
    (load-file "~/.emacs.d/myLisp/namespace.el"))
  :hook
  (csharp-mode-hook . my-csharp-mode-setup)
  :mode
  ("\\.cs\\$" . csharp-mode))

;; ** web-mode
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

  :hook (web-mode-hook . jacob-web-mode-config)

  :mode (("\\.html?\\'" . web-mode)
         ("\\.cshtml\\'" . web-mode)
         ("\\.css\\'" . web-mode)))
;; ** json-mode
(use-package json-mode
  :ensure t
  :mode ("\\.json\\$" . json-mode))
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
  :hook php-mode-hook
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
  (((web-mode-hook python-mode-hook java-mode-hook csharp-mode-hook php-mode-hook) . yas-minor-mode))

  :config
  (yas-reload-all))

;; ** key-chord
(use-package key-chord
  :defer 1

  :config
  (key-chord-mode 1))
;; ** outshine
(use-package outshine
  :ensure t
  :hook (emacs-lisp-mode-hook . outshine-mode))
;; * Non-mode Packages
;; ** try
(use-package try
  :ensure t
  :defer 5)
;; ** avy
(use-package avy
  :ensure t
  :defer 0.1
  :config
  (setq avy-keys (number-sequence ?a ?z))
  (setq avy-all-windows t)
  (setq avy-orders-alist
        '((avy-goto-char-timer . avy-order-closest)
          (avy-goto-end-of-line . avy-order-closest)))
  (key-chord-define xah-fly-key-map "fj" 'avy-goto-word-or-subword-1)
  (key-chord-define xah-fly-key-map "fk" 'avy-goto-end-of-line))
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

;; ** ivy and friends
;; *** ivy
(use-package ivy
  :ensure t
  :defer 0.1
  :config
  (setq ivy-initial-inputs-alist nil)
  (setq enable-recursive-minibuffers t)
  (setq completing-read-function 'ivy-completing-read)
  :bind
  (:map xah-fly-c-keymap
        ("e" . counsel-find-file))
  (:map xah-fly-dot-keymap
        ("s" . swiper))
  (:map xah-fly-h-keymap
        ("j" . counsel-describe-function)
        ("l" . counsel-describe-variable))
  (:map xah-fly-leader-key-map
        ("v" . counsel-yank-pop)
        ("f" . ivy-switch-buffer)))

;; *** swiper
(use-package swiper
  :ensure t
  :after ivy)

;; *** counsel
(use-package counsel
  :ensure t
  :after ivy)
;; ** expand-region
(use-package expand-region
  :ensure t
  
  :config
  (setq expand-region-contract-fast-key "9"))

;; ** shell-pop
(use-package shell-pop
  :ensure t

  :config
  (setq shell-pop-autocd-to-working-dir nil)
  (setq shell-pop-shell-type (quote ("eshell" "*eshell*" (lambda nil (eshell)))))
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
    (let ((shell-file-name "C:/Windows/System32/Cmd.exe")
          (shell-pop-shell-type '("shell" "*shell*" (lambda () (shell))))
          (shell-pop-term-shell "shell"))
      (shell-pop--set-shell-type 'shell-pop-shell-type shell-pop-shell-type)
      (call-interactively 'shell-pop)))

  :bind
  (:map xah-fly-n-keymap
        ("d" . jacob-shell-pop-eshell)
        ("f" . jacob-shell-pop-shell)))
;; ** amx
(use-package amx
  :ensure t
  :defer 1
  :config
  (amx-mode 1))

