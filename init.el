;;; init.el --- Jacob's main init file. -*-lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; `no-littering' needs to be loaded ASAP
(use-package no-littering)

;; constants and variables

(defconst jacob-megabyte (* 1024 1024)
  "Number of bytes in a megabyte.")

(defconst jacob-lisp-directory
  (file-name-concat (file-name-directory user-init-file)
                    "lisp")
  "Directory for my Lisp packages.")

(defconst jacob-environment-file
  (file-name-concat (file-name-directory user-init-file)
                    "environment.el")
  "File that holds additional configuration for that specific computer.")

(defconst jacob-is-windows (eq system-type 'windows-nt)
  "Is the current OS windows?")

(defconst jacob-is-linux (eq system-type 'gnu/linux)
  "Is the current OS linux?")

(defconst jacob-is-android (eq system-type 'android)
  "Is the current OS android?")

(defconst jacob-is-mac (eq system-type 'darwin)
  "Is the current OS a mac?")

(defvar jacob-is-fast nil
  "Is the device running Emacs fast?

Setting this to a non-nil value will cause different features to be loaded.")

(add-to-list 'load-path jacob-lisp-directory)
(add-to-list 'custom-theme-load-path jacob-lisp-directory)

(require 'jacob-init-helpers)
(require 'jacob-autoloads)

;; read custom file and environment file

(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file)

(condition-case error
    (load-file jacob-environment-file)
  (error
   (display-warning 'jacob
                    (format "error loading environment file %s"
                            error)
                    :error)))

;; configure packages

(use-package use-package
  :config
  (when (member "--report" command-line-args)
    (setq command-line-args (remove "--report" command-line-args))
    (setq use-package-compute-statistics t)
    (add-hook 'after-init-hook #'use-package-report)))

(use-package use-package-core
  :defer t
  :custom ((use-package-enable-imenu-support t)
           (use-package-hook-name-suffix nil)
           (use-package-verbose nil)))

(use-package jacob-use-package
  :config
  (setq use-package-keywords (append (seq-subseq use-package-keywords 0 2)
                                     (list :jacob-ensure-safely)
                                     (seq-subseq use-package-keywords 2))))

(use-package emacs
  :config
  ;; c source code
  (put 'narrow-to-region 'disabled nil)
  (setq-default bidi-display-reordering t
                bidi-paragraph-direction 'left-to-right)
  (setq bidi-inhibit-bpa t)
  (setq redisplay-skip-fontification-on-input t)

  ;; mule-cmds.el
  (prefer-coding-system 'utf-8)


  ;; custom hooks TODO: move to package

  (defvar jacob-first-minibuffer-activation-hook '()
    "Hook for first time minibuffer activated.")

  (defun jacob-run-first-minibuffer-activation-hook (&rest _args)
    "Run `jacob-first-minibuffer-activation-hook';
then remove this function from `find-file-hook'."
    (when (member 'init features)
      (run-hooks 'jacob-first-minibuffer-activation-hook)
      (advice-remove #'completing-read
                     #'jacob-run-first-minibuffer-activation-hook)))

  (advice-add #'completing-read
              :before
              #'jacob-run-first-minibuffer-activation-hook)

  :custom (
           ;; c source code
           (completion-ignore-case t)
           (create-lockfiles nil)
           (delete-by-moving-to-trash t)
           (echo-keystrokes (cond (jacob-is-android 1)
                                  (t 0.01)))
           (enable-recursive-minibuffers t)
           (frame-resize-pixelwise t)
           (history-delete-duplicates t)
           (history-length 1000)
           (kill-buffer-query-functions
            (remove 'process-kill-buffer-query-function kill-buffer-query-functions))
           (mode-line-format '("%e"
                               mode-line-front-space
                               mode-line-modified
                               mode-line-frame-identification
                               mode-line-buffer-identification
                               mode-line-position
                               (project-mode-line project-mode-line-format)
                               (vc-mode vc-mode)
                               " "
                               mode-line-modes
                               mode-line-format-right-align
                               mode-line-misc-info
                               mode-line-end-spaces))
           (read-process-output-max jacob-megabyte)
           (ring-bell-function 'ignore)
           (scroll-conservatively 101) ; Scroll just enough text to bring point into view.
           (tab-width 4) ; Set default tab char's display width to 4 spaces.
           (truncate-lines (cond (jacob-is-android t)
                                 (t nil)))
           (truncate-partial-width-windows nil)
           (use-dialog-box t)
           (use-short-answers t)
           (window-combination-resize t)
           ;; bindings.el
           (mode-line-percent-position nil)
           ;; indent.el
           (tab-always-indent 'complete) ; first try completion, then indent
           ;; startup.el
           (inhibit-startup-screen t)
           (initial-major-mode #'fundamental-mode)
           (initial-scratch-message
            (format
             ";; %s\n\n"
             (seq-random-elt
              '("\"A journey of a thousand miles begins with a single step.\" - 老子"
                "\"apex predator of grug is complexity\" - some grug"
                "\"Perfection is achieved, not when there is nothing more to add, but when there is nothing left to take away.\" - Antoine de Saint-Exupéry"
                "\"Always listen to Jiaqi.\" - Jacob Leeming"
                "\"The king wisely had the computer scientist beheaded, and they all lived happily ever after.\" - anon"
                "\"Success is going from failure to failure without losing your enthusiasm.\" - Winston Churchill (maybe)"))))
           ;; lisp.el
           (parens-require-spaces nil)
           (delete-pair-blink-delay 0)
           ;; paragraphs.el
           (sentence-end-double-space nil))
  :bind (("C-M-k" . jacob-kill-sexp)    ; `kill-sexp'
         :map mode-line-buffer-identification-keymap
         ("<mode-line> <mouse-2>" . ibuffer)))

(use-package package
  :defer t
  :config
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
  :custom ((package-install-upgrade-built-in t)))

(use-package jacob-editing-commands
  :demand
  :bind (("DEL" . jacob-delete-backwards)  ; `delete-backward-char'
         ("C-k" . jacob-kill-line)         ; `kill-line'
         ("C-a" . jacob-beginning-of-line) ; `beginning-of-line'
         ("C-e" . jacob-end-of-line)       ; `end-of-line'
         ))

(use-package jacob-modal-editing
  :when (not jacob-is-android)
  :functions (jacob-modal-editing-mode)
  :config
  (require 'jacob-modal-editing-config)
  (jacob-modal-editing-mode 1))

(use-package cus-edit
  :defer t
  :config
  (advice-add #'custom-save-all :after #'jacob-format-custom-file))

(use-package menu-bar
  :config
  (keymap-global-unset "<menu-bar> <file> <make-frame-on-display>")
  (keymap-global-unset "<menu-bar> <file> <open-file>")
  (keymap-global-unset "<menu-bar> <file> <recover-session>")
  (keymap-global-unset "<menu-bar> <file> <insert-file>")
  (keymap-global-unset "<menu-bar> <file> <make-frame-on-monitor>")
  (keymap-global-unset "<menu-bar> <file> <write-file>")
  (keymap-global-unset "<menu-bar> <file> <print>")
  (keymap-global-set "<menu-bar> <file> <restart>" '("Restart" . restart-emacs))

  (keymap-global-set "<menu-bar> <edit> <goto> <imenu>" '("Imenu" . imenu))

  (keymap-global-set "<menu-bar> <tools> <magit>" '("Magit" . magit))
  (keymap-global-set "<menu-bar> <tools> <org-agenda>" '("Agenda" . org-agenda))
  (keymap-global-set "<menu-bar> <tools> <gptel>" '("Gptel" . gptel))
  (keymap-global-set "<menu-bar> <tools> <dired>" '("Dired" . dired-jump)))

(use-package tool-bar
  :config
  (when jacob-is-android
    (tool-bar-mode 1))
  :custom ((tool-bar-button-margin (if jacob-is-android 39 4))
           (tool-bar-position (if jacob-is-android 'bottom 'top))
           (tool-bar-style 'image)))

(use-package jacob-tool-bar
  :when jacob-is-android
  :functions (jacob-tool-bar-setup)
  :config
  (jacob-tool-bar-setup))

;; TODO: bring all uses of `on' into its declaration. how can i make
;; this fault tolerant? if this package is unavailable, then we won't
;; be able to load the other packages which rely on its hooks to load.

;; TODO: move wrapper hooks somewhere appropriate

;; TODO: on input
;; TODO: on init ui

(defvar jacob-on-first-file-wrapper-hook nil
  "Wrapper hook for `on-first-file-hook'.

The idea is if the `on' package is unavailable, we can eagerly call this
hook so that functionality outside of `on' is unaffected.

Elsewhere in the init file, do not use `on' directly, instead use this
and similar hooks.")

(defun jacob-run-first-file-wrapper-hook ()
  "Run `jacob-on-first-file-wrapper-hook' hooks."
  (run-hooks 'jacob-on-first-file-wrapper-hook))

(defvar jacob-on-first-input-wrapper-hook nil
  "Wrapper hook for `on-first-input-hook'.

The idea is if the `on' package is unavailable, we can eagerly call this
hook so that functionality outside of `on' is unaffected.

Elsewhere in the init file, do not use `on' directly, instead use this
and similar hooks.")

(defun jacob-run-first-input-wrapper-hook ()
  "Run `jacob-on-first-input-wrapper-hook' hooks."
  (message "%s" "run first input hook")
  (run-hooks 'jacob-on-first-input-wrapper-hook))

(defun jacob-handle-on-unavailable ()
  "Handle `on' being unavailable.

When the package `on' is unavailable, run the wrapper hooks to ensure
functionality outside of `on' is not lost."
  (unless (boundp 'on-first-input-hook)
    (message "%s" "on-first-input-hook unavailable")
    (jacob-run-first-input-wrapper-hook))
  (unless (boundp 'on-first-file-hook)
    (message "%s" "on-first-file-hook unavailable")
    (jacob-run-first-file-wrapper-hook)))

(add-hook 'after-init-hook #'jacob-handle-on-unavailable)

;; (jacob-run-first-input-wrapper-hook)           ; temp hack
;; (jacob-run-first-file-wrapper-hook)           ; temp hack

(use-package on
  ;; :jacob-ensure-safely t ; FIXME this is causing on to not load when the package is installed
  :demand t ; we don't want to defer this
  :hook ((on-first-file-hook . jacob-run-first-file-wrapper-hook)
         (on-first-input-hook . jacob-run-first-input-wrapper-hook)))

(use-package blackout
  :functions (blackout)
  :config
  (with-eval-after-load 'which-key
    (blackout 'which-key-mode))
  (with-eval-after-load 'autorevert
    (blackout 'auto-revert-mode))
  (with-eval-after-load 'editorconfig
    (blackout 'editorconfig-mode))
  (with-eval-after-load 'yasnippet
    (blackout 'yas-minor-mode " yas"))
  (with-eval-after-load 'subword
    (blackout 'subword-mode))
  (with-eval-after-load 'nerd-icons-dired
    (blackout 'nerd-icons-dired-mode))
  (with-eval-after-load 'eldoc
    (blackout 'eldoc-mode))
  (with-eval-after-load 'hi-lock
    (blackout 'hi-lock-mode))
  (with-eval-after-load 'apheleia
    (blackout 'apheleia-mode " ⚘"))
  (with-eval-after-load 'rainbow-mode
    (blackout 'rainbow-mode))
  (with-eval-after-load 'stripspace
    (blackout 'stripspace-local-mode)))

(use-package which-key
  :if jacob-is-fast
  :hook ((jacob-on-first-input-wrapper-hook . which-key-mode))
  :custom ((which-key-idle-delay (cond (jacob-is-android 1)
                                       (t 0.01)))))

(use-package mouse
  :hook ((jacob-on-first-input-wrapper-hook . context-menu-mode))
  :custom ((mouse-1-double-click-prefer-symbols t)
           (mouse-drag-copy-region 'non-empty)))

(use-package touch-screen
  :defer t
  :custom ((touch-screen-display-keyboard nil)
           (touch-screen-extend-selection t)
           (touch-screen-preview-select t)
           (touch-screen-word-select t)
           (touch-screen-set-point-commands nil)))

(use-package modus-themes
  :custom ((modus-themes-to-toggle '(modus-operandi-tinted modus-vivendi-tinted))))

(use-package mwheel
  :custom ((mouse-wheel-scroll-amount '(10 ((shift) . hscroll)
                                           ((meta))
                                           ((control) . text-scale)))))

(use-package tooltip
  :custom ((tooltip-delay (cond (jacob-is-android 0.7)
                                (t 0.1)))))

(use-package files
  :hook ((jacob-on-first-file-wrapper-hook . auto-save-visited-mode))
  :custom ((auto-save-default nil)
           (auto-save-visited-interval 2) ; Save file after two seconds.
           (backup-by-copying t)
           (confirm-kill-processes nil)
           (make-backup-files nil)
           (remote-file-name-inhibit-auto-save-visited t)))

(use-package files-x
  :config
  (connection-local-set-profile-variables
   'remote-direct-async-process
   '((tramp-direct-async-process . t)))

  (connection-local-set-profiles
   '(:application tramp :protocol "scp")
   'remote-direct-async-process))

(use-package autorevert
  :hook ((jacob-on-first-file-wrapper-hook . global-auto-revert-mode)))

(use-package window
  :bind ( :repeat-map jacob-window-repeat-map
          ("b" . consult-buffer)
          ("o" . other-window))
  :custom ((display-buffer-alist '(((major-mode . sql-interactive-mode)
                                    (display-buffer-reuse-mode-window
                                     display-buffer-same-window))
                                   ((major-mode . prodigy-mode)
                                    (display-buffer-reuse-mode-window
                                     display-buffer-same-window))
                                   ((major-mode . magit-status-mode)
                                    (display-buffer-reuse-mode-window
                                     display-buffer-same-window))
                                   ((or (derived-mode . slack-mode)
                                        (derived-mode . lui-mode))
                                    (display-buffer-in-side-window)
                                    (side . right))))
           (split-height-threshold nil)
           (switch-to-buffer-obey-display-actions t)))

(defvar-keymap jacob-recenter-repeat-map
  :repeat t
  "p" #'recenter-top-bottom)

(use-package frame
  :config
  (blink-cursor-mode 0)
  :custom ((blink-cursor-blinks 0)      ; make cursor blink forever
           )
  :bind ("C-z" . nil)                 ; `suspend-frame'
  )

(use-package novice
  :defer t
  :config
  (setq disabled-command-function nil))

(use-package recentf
  :hook ((jacob-on-first-input-wrapper-hook . recentf-mode))
  :custom ((recentf-max-saved-items nil)))

(use-package savehist
  :hook ((jacob-first-minibuffer-activation-hook . savehist-mode))
  :custom ((savehist-additional-variables '(search-ring regexp-search-ring kill-ring))
           (savehist-save-minibuffer-history t)))

(use-package saveplace
  :hook ((jacob-on-first-file-wrapper-hook . save-place-mode))
  :custom ((save-place-forget-unreadable-files t)))

(use-package generic-x           ; support for files like `/etc/fstab'
  :defer t)

(use-package conf-mode
  :bind ( :map conf-mode-map
          ("C-c SPC" . nil)))

(use-package simple
  :hook ((on-init-ui-hook . column-number-mode)
         (on-init-ui-hook . line-number-mode))
  :bind (("C-x u" . nil)                ; `undo'
         )
  :config
  (put 'set-goal-column 'disabled nil)
  :custom ((indent-tabs-mode nil)       ; use spaces to indent
           (kill-do-not-save-duplicates t)
           (read-extended-command-predicate 'command-completion-default-include-p)
           (save-interprogram-paste-before-kill t)))

(use-package thingatpt
  :defer t
  :config
  (require 'jacob-thingatpt))

(use-package timer-list
  :defer t
  :config
  (put 'list-timers 'disabled nil))

(use-package misc
  :bind ("M-z" . zap-up-to-char))       ; `zap-to-char'

(use-package isearch
  :bind ( :map isearch-mode-map
          ("<right>" . isearch-repeat-forward)
          ("<left>" . isearch-repeat-backward))
  :custom ((isearch-lazy-count t)))

(use-package re-builder
  :defer t
  :custom ((reb-re-syntax 'string)))

(use-package bookmark
  :defer t
  :config
  (require 'jacob-bookmark)
  :custom ((bookmark-fringe-mark nil)
           (bookmark-watch-bookmark-file 'silent)))

(use-package dabbrev
  :defer t
  :custom ((dabbrev-case-fold-search nil)
           (dabbrev-case-replace nil)))

(use-package hippie-exp
  :defer t
  :custom ((hippie-expand-try-functions-list (remove 'try-expand-list
                                                     hippie-expand-try-functions-list))))

(use-package editorconfig
  :hook ((prog-mode-hook . editorconfig-mode)))

(use-package flymake
  :bind
  ("M-n" . flymake-goto-next-error)
  ("M-p" . flymake-goto-prev-error))

(use-package consult-project-extra
  :defer t
  :functions (consult-project-extra-find)
  :init
  (keymap-set project-prefix-map "f" #'consult-project-extra-find)
  :custom ((consult-project-function 'consult-project-extra-project-fn)))

(use-package project
  :defer t
  :custom ((project-compilation-buffer-name-function 'project-prefixed-buffer-name)
           (project-switch-use-entire-map t)))

(use-package jacob-project
  :after project
  :bind ( :map project-prefix-map
          ("t" . jacob-project-visit-test))
  :hook ((project-find-functions . jacob-project-try-exercism)))

(use-package yasnippet
  :hook ((snippet-mode . yas-minor-mode)
         (snippet-mode . jacob-disable-auto-save-in-buffer))
  :bind ( :map yas-minor-mode-map
          ("C-c y n" . yas-new-snippet)
          ("C-c y v" . yas-visit-snippet-file)
          ("C-c y i" . yas-insert-snippet))
  :functions (yas-reload-all)
  :config
  (require 'jacob-yasnippet)
  (yas-reload-all)
  :custom ((yas-new-snippet-default "# -*- mode: snippet -*-
# key: $1
# --
$0")
           (yas-wrap-around-region t)))

(use-package minibuffer
  :bind ( :map minibuffer-local-completion-map
          ("SPC" . self-insert-command))
  :custom ((completion-styles '(orderless basic initials))
           (completion-at-point-functions nil) ; Remove the default tags based backend
           (completion-category-overrides '((file (styles basic partial-completion))))
           (completion-auto-help 'always)
           (completion-auto-select 'second-tab)
           (completions-format 'one-column)))

(use-package mb-depth
  :hook ((jacob-first-minibuffer-activation-hook . minibuffer-depth-indicate-mode)))

(use-package man
  :defer t
  :custom ((Man-notify-method 'pushy)))

(use-package help-fns
  :defer t
  :config
  (require 'jacob-help-fns)
  (put 'help-fns-edit-variable 'disabled nil)
  :custom ((help-enable-variable-value-editing t)
           (help-window-select t)))

(use-package subword
  :hook ((jacob-on-first-input-wrapper-hook . global-subword-mode)))

(use-package paren
  :hook ((jacob-on-first-input-wrapper-hook . show-paren-mode))
  :custom ((show-paren-context-when-offscreen 'echo)))

(use-package electric
  :defer t
  :custom ((electric-indent-mode nil) ; Enabled by default.
           ))

(use-package elec-pair
  :hook ((jacob-on-first-input-wrapper-hook . electric-pair-mode)))

(use-package puni
  :bind (("M-d" . puni-forward-kill-word) ; `kill-word'
         ("M-DEL" . puni-backward-kill-word) ; `backward-kill-word'
         ("C-M-f" . puni-forward-sexp-or-up-list) ; `forward-sexp'
         ("C-M-b" . puni-backward-sexp-or-up-list) ; `backward-sexp'
         )
  :config
  (require 'jacob-puni))

(use-package delsel
  :hook ((jacob-on-first-input-wrapper-hook . delete-selection-mode)))

(use-package repeat
  :hook ((jacob-on-first-input-wrapper-hook . repeat-mode)))

(use-package vc-hooks
  :defer t
  :custom ((vc-ignore-dir-regexp
            (format "\\(%s\\)\\|\\(%s\\)"
                    locate-dominating-stop-dir-regexp
                    tramp-file-name-regexp)) ; Disable vc functionality in tramp files.
           ))

(use-package magit-mode
  :defer t
  :config
  (unless (string-match-p "^\\*.+\\*$" magit-buffer-name-format)
    (setq magit-buffer-name-format
          (format "*%s*" magit-buffer-name-format))))

(use-package magit-extras
  :defer t
  :init
  (when jacob-is-fast
    (keymap-set project-prefix-map "v" #'magit-project-status)))

(use-package magit-section
  :defer t
  :custom ((magit-section-initial-visibility-alist '((untracked . show) (stashes . show)))))

(use-package magit-process
  :defer t
  :config
  (setq magit-tramp-pipe-stty-settings 'pty))

(use-package autoinsert
  :hook ((jacob-on-first-file-wrapper-hook . auto-insert-mode))
  :config
  (jacob-define-auto-insert "\\.el$" ["template.el" checkdoc elisp-enable-lexical-binding])
  (jacob-define-auto-insert "\\.scala$" ["template.scala" jacob-autoinsert-yas-expand])
  (jacob-define-auto-insert "\\.cs$" ["template.cs" jacob-autoinsert-yas-expand])
  (jacob-define-auto-insert "Controller\\.cs$" ["controllerTemplate.cs" jacob-autoinsert-yas-expand])
  :custom ((auto-insert-query nil)))

(use-package tramp
  :defer t
  :config
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path)
  (setq tramp-archive-enabled nil) ; lots of problems. for now, disable it!
  )

(use-package tramp-sh
  :defer t
  :custom ((tramp-copy-size-limit jacob-megabyte)
           (tramp-use-scp-direct-remote-copying t)))

(use-package tramp-integration
  :defer t
  :functions (tramp-compile-disable-ssh-controlmaster-options)
  :config
  (with-eval-after-load 'compile
    (remove-hook 'compilation-mode-hook #'tramp-compile-disable-ssh-controlmaster-options)))

(use-package dumb-jump
  :defer t
  :functions (dumb-jump-xref-activate)
  :config
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
  (when jacob-is-mac
    ;; problems on mac mean git grep doesn't work :(
    (setq-default dumb-jump-force-searcher 'grep)))

(use-package eglot
  :bind
  (("C-c e e" . eglot)
   :map eglot-mode-map
   ("C-c e a" . eglot-code-actions)
   ("C-c e i" . eglot-find-implementation)
   ("C-c e r" . eglot-rename)
   ("C-c e t" . eglot-find-typeDefinition)
   ("C-c e h" . eglot-inlay-hints-mode)
   ("C-c e o" . eglot-code-action-organize-imports)
   ("C-c e y" . jacob-eglot-yank))
  :functions (eglot-semantic-tokens-mode
              eglot-alternatives
              eglot-flymake-backend)
  :config
  (jacob-defhookf eglot-managed-mode-hook
    (eglot-inlay-hints-mode 0)
    (eglot-semantic-tokens-mode 0)
    (setq-local xref-backend-functions '(eglot-xref-backend dumb-jump-xref-activate t))
    (add-hook 'flymake-diagnostic-functions #'eglot-flymake-backend nil "LOCAL"))

  (setq-default eglot-workspace-configuration '(:metals ( :inlayHints ( :implicitArguments (:enable t)
                                                                        :implicitConversions (:enable t))
                                                          :autoImportBuilds "all"
                                                          :targetBuildTool "sbt"
                                                          :defaultBspToBuildTool t
                                                          :enableBestEffort t)))

  (add-to-list 'eglot-server-programs '((csharp-mode csharp-ts-mode) . (lambda (_interactive _project)
                                                                         "Don't activate eglot when in a C# script."
                                                                         (unless (string= (file-name-extension (buffer-name (current-buffer)))
                                                                                          "csx")
                                                                           '("csharp-ls")))))

  (add-to-list 'eglot-server-programs '(sql-mode . "sqls"))

  (add-to-list 'eglot-server-programs `((js-mode
                                         js-ts-mode
                                         tsx-ts-mode
                                         (typescript-ts-base-mode :language-id "typescript")
                                         typescript-mode)
                                        . ,(eglot-alternatives
                                            '(("typescript-language-server" "--stdio")
                                              ("deno" "lsp"
                                               :initializationOptions
                                               (:enable t :lint t :suggest.names t))))))

  (add-to-list 'eglot-server-programs '((gdscript-mode gdscript-ts-mode) "localhost" 6008))

  (add-to-list 'eglot-server-programs '(scala-ts-mode . ("metals" :initializationOptions (:isHttpEnabled t))))

  (eglot--code-action eglot-code-action-organize-imports-ts "source.organizeImports.ts")
  (eglot--code-action eglot-code-action-add-missing-imports-ts "source.addMissingImports.ts")

  (setopt eglot-ignored-server-capabilities '(:documentOnTypeFormattingProvider :documentFormattingProvider)
          eglot-stay-out-of '(imenu flymake)
          eglot-code-action-indications '(eldoc-hint mode-line)))

(use-package jacob-eglot
  :defer t
  :after (eglot)
  :functions (jacob-eglot-xref-backend
              jacob-eglot-remove-ret-character-from-buffer)
  :config
  (advice-add 'eglot-xref-backend :override #'jacob-eglot-xref-backend)
  (advice-add 'eglot-code-actions :after #'jacob-eglot-remove-ret-character-from-buffer)
  (advice-add 'eglot-rename :after #'jacob-eglot-remove-ret-character-from-buffer))

(require 'jacob-csharp-mode)

(require 'jacob-sharper)

(use-package csproj-mode
  :mode ("\\.csproj\\'" . csproj-mode))

(use-package sln-mode
  :mode ("\\.sln\\'" . sln-mode))

(use-package fsharp-mode
  :mode ("\\.fs\\'" . fsharp-mode)
  :functions (fsharp-mode-project-root)
  :config
  (remove-hook 'project-find-functions #'fsharp-mode-project-root)
  (setopt compilation-error-regexp-alist (remove 'fsharp compilation-error-regexp-alist)))

(use-package scala-mode
  :hook ((scala-mode-hook . apheleia-mode)
         (scala-mode-hook . yas-minor-mode)
         (scala-mode-hook . electric-indent-local-mode)
         (scala-mode-hook . jacob-trim-quotes-mode)
         (scala-mode-hook . eglot-ensure)
         (scala-mode-hook . flymake-mode)
         (scala-mode-hook . stripspace-local-mode)))

(use-package scala-mode-map
  :defines (scala-mode-map)
  :bind ( :map scala-mode-map
          ("$" . jacob-scala-dollar)
          ("." . jacob-scala-.)))

(use-package sbt-mode
  :hook ((sbt-mode-hook . compilation-shell-minor-mode))
  :functions (sbt:initialize-for-compilation-mode)
  :config
  (advice-add #'sbt:initialize-for-compilation-mode :override #'ignore))

(use-package jacob-scala
  :hook ((scala-mode-hook . jacob-scala-font-lock-setup)
         (scala-mode-hook . jacob-scala-setup-flymake))
  :bind ( :map project-prefix-map
          ("S" . jacob-project-sbt)))

(use-package web-mode
  :mode ("\\.scala\\.html\\'" . web-mode)
  :custom ((web-mode-engines-alist
            '(("play" . "\\.scala\\.html\\'"))))
  :functions (web-mode-indent-line)
  :config
  ;; patch web-mode-indent-line so that '}' is indented properly
  (advice-patch #'web-mode-indent-line
                '((and (string= web-mode-engine "razor")
                       (string-match-p "^\\([{}]\\|else\\)" curr-line))
                  (when debug (message "I142(%S) razor" pos))
                  (if (string= "}" curr-line)
                      (save-excursion
                        (search-forward "}")
                        (backward-sexp)
                        (setq offset (current-indentation)))
                    (save-excursion
                      (web-mode-block-previous)
                      (setq offset (current-indentation)))))
                '((and (string= web-mode-engine "razor")
                       (string-match-p "^\\([{}]\\|else\\)" curr-line))
                  (when debug (message "I142(%S) razor" pos))
                  (save-excursion
                    (web-mode-block-previous)
                    (setq offset (current-indentation))))))

(use-package ls-lisp
  :defer t
  :init
  (setq ls-lisp-use-insert-directory-program nil
        ls-lisp-dirs-first t))

(use-package dired
  :hook ((dired-mode-hook . dired-hide-details-mode)
         (dired-mode-hook . auto-revert-mode))
  :custom ((dired-recursive-copies 'always)
           (dired-dwim-target t)
           (dired-listing-switches "-hal") ; the h option needs to come first 🙃
           (dired-guess-shell-alist-user '(("\\.mkv\\'" "mpv")
                                           ("\\.mp4\\'" "mpv")))))

(use-package dired-x
  :defer t
  :after dired)

(use-package dired-aux
  :defer t
  :config
  (setopt dired-vc-rename-file t))

(use-package dired-rsync
  :defer t
  :after dired
  :config
  (add-to-list 'mode-line-misc-info '(:eval dired-rsync-modeline-status 'append)))

(use-package nerd-icons-dired
  :when (and (display-graphic-p)
             (not jacob-is-android))
  :hook ((dired-mode-hook . nerd-icons-dired-mode)))

(use-package nerd-icons-mode-line
  :when (and (display-graphic-p)
             (not jacob-is-android))
  :hook ((jacob-on-first-file-wrapper-hook . nerd-icons-mode-line-global-mode)))

(use-package nerd-icons-completion
  :jacob-ensure-safely t
  :when (and (display-graphic-p)
             (not jacob-is-android))
  :hook ((prog-mode-hook . nerd-icons-completion-mode)))

(use-package nerd-icons-grep
  :when (and (display-graphic-p)
             (not jacob-is-android))
  :hook ((grep-mode-hook . nerd-icons-grep-mode)))

(use-package nerd-icons-xref
  :when (and (display-graphic-p)
             (not jacob-is-android))
  :hook ((xref--xref-buffer-mode-hook . nerd-icons-xref-mode)))

(use-package nerd-icons-ibuffer
  :when (and (display-graphic-p)
             (not jacob-is-android))
  :hook ((ibuffer-mode-hook . nerd-icons-ibuffer-mode)))

(use-package esh-mode
  :defer t
  :custom ((eshell-scroll-to-bottom-on-output 'this)))

(use-package jacob-eshell
  :after (esh-mode)
  :functions (jacob-eshell-windows-confirm-terminate-batch-job)
  :config
  (when jacob-is-windows
    (advice-add 'eshell-interrupt-process
                :after
                #'jacob-eshell-windows-confirm-terminate-batch-job)))

(use-package imenu
  :custom ((imenu-use-popup-menu 'on-mouse)))

(use-package eldoc
  :hook ((prog-mode-hook . global-eldoc-mode))
  :config
  (setopt eldoc-documentation-strategy 'eldoc-documentation-compose))

(use-package consult-git-log-grep
  :bind ( :map project-prefix-map
          ("l" . consult-git-log-grep)))

(use-package prodigy
  :hook ((prodigy-view-mode-hook . (lambda ()
                                     "Disable view mode"
                                     (view-mode 0)))
         (prodigy-view-mode-hook . compilation-minor-mode))
  :bind ( :map project-prefix-map
          ("L" . prodigy))
  :functions (prodigy-define-tag prodigy-set-status)
  :config
  (prodigy-define-tag
    :name 'asp.net
    :stop-signal 'kill
    :on-output (lambda (&rest args)
                 (let ((output (plist-get args :output))
                       (service (plist-get args :service)))
                   (when (string-match-p "Hosting started *$" output)
                     (prodigy-set-status service 'ready)))))

  (prodigy-define-tag
    :name 'sbt
    :command "sbt"
    :args '("run" "-Dapplication.router=testOnlyDoNotUseInAppConf.Routes")
    :ready-message "(Server started, use Enter to stop and go back to the console...)"))

(use-package hl-todo
  :hook ((after-init-hook . global-hl-todo-mode)))

(defun jacob-font-lock-programming-setup ()
  "Setup faces locally for programming."
  (dolist (face '(font-lock-type-face
                  font-lock-keyword-face
                  font-lock-variable-use-face
                  font-lock-function-call-face
                  font-lock-preprocessor-face
                  font-lock-property-use-face
                  font-lock-builtin-face))
    (face-remap-add-relative face
                             :foreground (face-foreground 'default)
                             :weight (face-attribute 'default :weight)))

  (face-remap-add-relative 'font-lock-comment-face
                           :inherit 'font-lock-warning-face))

(use-package lisp-mode
  :bind ( :map lisp-mode-shared-map
          ("DEL" . nil)              ; `backward-delete-char-untabify'
          ))

(use-package elisp-mode
  :hook ((emacs-lisp-mode-hook . apheleia-mode)
         (emacs-lisp-mode-hook . jacob-font-lock-programming-setup)
         (emacs-lisp-mode-hook . yas-minor-mode)
         (emacs-lisp-mode-hook . stripspace-local-mode)
         (emacs-lisp-mode-hook . electric-indent-local-mode)
         (emacs-lisp-mode-hook . flymake-mode))
  :config
  (keymap-set emacs-lisp-mode-menu "<eval-defun>" '("Evaluate definition" . eval-defun))
  (jacob-defhookf emacs-lisp-mode-hook
    (setq-local yas-key-syntaxes '("w_"))
    (add-hook 'flymake-diagnostic-functions
              #'jacob-elisp-flymake-check-removals nil "LOCAL")
    (add-hook 'flymake-diagnostic-functions
              #'jacob-flymake-use-package nil "LOCAL")
    (add-to-list 'completion-at-point-functions #'cape-dabbrev))

  (setopt elisp-flymake-byte-compile-load-path load-path)

  (font-lock-add-keywords 'emacs-lisp-mode
                          '(("(use-package \\([[:word:]-]+\\)" 1 'font-lock-function-name-face))))

(use-package jacob-elisp
  :after elisp-mode
  :bind ( :map lisp-interaction-mode-map
          ("C-j" . jacob-eval-print-last-sexp))
  :functions (jacob-elisp-move-past-close-and-reindent)
  :config
  (advice-add #'move-past-close-and-reindent :after #'jacob-elisp-move-past-close-and-reindent))

(use-package scheme
  :mode ("\\.scm\\'" . scheme-mode)
  :config
  (keymap-set scheme-mode-map "(" #'insert-parentheses)
  (keymap-set scheme-mode-map ")" #'move-past-close-and-reindent))

(use-package fennel-mode
  :hook ((fennel-mode-hook . yas-minor-mode)
         (fennel-mode-hook . apheleia-mode)
         (fennel-mode-hook . electric-indent-mode)
         (fennel-mode-hook . stripspace-local-mode)))

(use-package geiser
  :after scheme)

(use-package geiser-guile
  :after (scheme geiser))

(use-package mermaid-mode
  :mode ("\\.mermaid\\'"))

(use-package ob-mermaid
  :after org)

(use-package org
  :mode ("\\.org\\'" . org-mode)
  :hook ((org-mode-hook . yas-minor-mode))
  :functions (org-backward-paragraph
              org-forward-paragraph
              org-link-set-parameters
              org-agenda-todo)
  :config
  (add-hook 'org-babel-post-tangle-hook #'jacob-org-babel-tangle-delete-whitespace)

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((octave . t)
     (sql . t)
     (js . t)
     (mermaid . t)))

  (setopt org-startup-folded t
          org-tags-column 0
          org-capture-templates '(("i" "Inbox" entry (file "") "* TODO %?\n:PROPERTIES:\n:CREATED: %U\n:END:")))

  (jacob-defhookf org-mode-hook
    (toggle-truncate-lines 0)
    (toggle-word-wrap 1)
    (setq-local jacob-backward-paragraph-function #'org-backward-paragraph)
    (setq-local jacob-forward-paragraph-function #'org-forward-paragraph))

  :custom ((org-default-notes-file "~/Documents/notes.org")
           (org-log-into-drawer t)))

(use-package jacob-org
  :defer t
  :after ol
  :functions (jacob-org-jira-follow jacob-org-project-follow)
  :config
  (org-link-set-parameters "jira"
                           :follow #'jacob-org-jira-follow)
  (org-link-set-parameters "project"
                           :follow #'jacob-org-project-follow))

(use-package org-agenda
  :hook ((org-agenda-mode-hook . hl-line-mode))
  :config
  (setopt org-agenda-custom-commands '(("r" "Routine" agenda "" ((org-agenda-tag-filter-preset '("+tickler"))
                                                                 (org-agenda-span 'day)))
                                       ("w" "Work" todo "" ((org-agenda-tag-filter-preset '("+work"))))
                                       ("j" "Jobs"
                                        agenda ""
                                        ((org-agenda-span 3)
                                         (org-agenda-start-day "-1d")
                                         (org-agenda-time-grid '((daily today require-timed)
                                                                 nil
                                                                 " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄"))
                                         (org-agenda-tag-filter-preset '("-tickler" "-work"))))))

  (defvar org-agenda-tool-bar-map
    (let ((map (make-sparse-keymap)))
      (tool-bar-local-item "checked"
                           (lambda ()
                             (interactive)
                             (org-agenda-todo 'done))
                           :done
                           map
                           :vert-only t)
      map))

  (jacob-defhookf org-agenda-mode-hook
    (setq-local tool-bar-map org-agenda-tool-bar-map))

  :bind (("C-c o a" . org-agenda)
         ("C-c o c" . org-capture)))

(defvar jacob-jira-url nil
  "Jira url for current project.")

(use-package org-src
  :after org
  :config
  (setopt org-src-preserve-indentation t))

(use-package org-compat
  :after org
  :config
  (setopt org-calendar-to-agenda-key nil  ; don't bind calendar key
          org-calendar-insert-diary-entry-key nil) ; don't bind calendar key
  )

(use-package org-cycle
  :defer t
  :custom ((org-cycle-separator-lines 0)))

(use-package ox-latex
  :after org
  :config
  (setopt org-latex-pdf-process (list "latexmk -pdf %f -shell-escape")) ; probably requires texlive
  )

;; (require 'ox-extra)

;; (ox-extras-activate '(latex-header-blocks ignore-headlines))

(use-package jacob-pulse
  :functions (jacob-pulse-jacob-line-content jacob-pulse-defun jacob-pulse-previous-sexp)
  :init
  (dolist (command '(recenter-top-bottom
                     scroll-up-command
                     scroll-down-command
                     other-window
                     jacob-split-or-switch-window
                     xref-find-definitions
                     xref-pop-marker-stack
                     isearch-done))
    (advice-add command :after #'jacob-pulse-jacob-line-content))

  (advice-add #'eval-defun :after #'jacob-pulse-defun)
  (advice-add #'eval-last-sexp :after #'jacob-pulse-previous-sexp))

(use-package server
  :hook ((after-init-hook . server-start)))

(use-package calendar
  :defer t
  :config
  (add-hook 'calendar-today-visible-hook 'calendar-mark-today)

  (setopt calendar-date-style 'european
          calendar-date-display-form '((if dayname
                                           (concat dayname ", "))
                                       day "/" month "/" year)
          calendar-week-start-day 1
          calendar-mark-holidays-flag t))

(use-package winner
  :commands (winner-undo winner-redo)
  :hook ((jacob-on-first-input-wrapper-hook . winner-mode)))

(use-package compile
  :defer t
  :init
  (keymap-global-set "<f5>" 'recompile)
  :config
  (require 'jacob-compile)
  (add-hook 'compilation-filter-hook #'ansi-color-compilation-filter)

  (add-to-list 'compilation-error-regexp-alist-alist
               '(jacob-sbt-test-fail-re
                 ".* (\\([a-zA-Z\\.]+\\):\\([0-9]+\\))"
                 jacob-compilation-project-file
                 2))

  (add-to-list 'compilation-error-regexp-alist 'jacob-sbt-test-fail-re)

  (add-to-list 'compilation-error-regexp-alist-alist
               '(jacob-sbt-stack-trace-re
                 "at .*(\\([a-zA-Z.]+\\):\\([0-9]+\\))"
                 jacob-compilation-project-file
                 2))

  (add-to-list 'compilation-error-regexp-alist 'jacob-sbt-stack-trace-re)

  (add-to-list 'compilation-error-regexp-alist-alist
               '(jacob-sbt-error
                 "^\\[error][[:space:]]--[[:space:]].*Error: \\([^:]+\\):\\([[:digit:]]+\\):\\([[:digit:]]+\\)" 1 2 3 nil 1))

  (add-to-list 'compilation-error-regexp-alist 'jacob-sbt-error)

  (add-to-list 'compilation-error-regexp-alist-alist
               '(jacob-sbt-warning
                 "^\\[warn][[:space:]]--[[:space:]].*Warning: \\([^:]+\\):\\([[:digit:]]+\\):\\([[:digit:]]+\\)" 1 2 3 1 1))

  (add-to-list 'compilation-error-regexp-alist 'jacob-sbt-warning)

  (dolist (re '(gcov-file gcov-header gcov-nomark gcov-called-line gcov-never-called guile-line guile-file))
    (setq compilation-error-regexp-alist (remove re compilation-error-regexp-alist)))

  (setq compilation-mode-font-lock-keywords '((" --?o\\(?:utfile\\|utput\\)?[= ]\\(\\S +\\)" . 1)
                                              ("^Compilation \\(finished\\).*"
                                               (0 '(face nil compilation-message nil help-echo nil mouse-face nil)
                                                  t)
                                               (1 compilation-info-face))
                                              ("^Compilation \\(exited abnormally\\|interrupt\\|killed\\|terminated\\|segmentation fault\\)\\(?:.*with code \\([0-9]+\\)\\)?.*"
                                               (0 '(face nil compilation-message nil help-echo nil mouse-face nil)
                                                  t)
                                               (1 compilation-error-face) (2 compilation-error-face nil t))
                                              ("error\\|ERROR"
                                               (0 compilation-error-face))
                                              ("\\(warn\\|WARN\\)\\(ing\\|ING\\)?"
                                               (0 compilation-warning-face))))
  :custom ((compilation-always-kill t)
           (compilation-scroll-output t)
           (compilation-ask-about-save nil)))

(use-package winnow
  :hook ((compilation-mode-hook . winnow-mode)))

(require 'jacob-sql)

(use-package treesit
  :defer t
  :custom ((treesit-font-lock-level 4)))
;; TODO: write a function that sorts the lines inside a list
(use-package treesit-auto
  :functions (treesit-auto-add-to-auto-mode-alist
              global-treesit-auto-mode)
  :config
  (add-to-list 'treesit-auto-recipe-list (make-treesit-auto-recipe
                                          :lang 'gdscript
                                          :ts-mode 'gdscript-ts-mode
                                          :remap 'gdscript-mode
                                          :url "https://github.com/PrestonKnopp/tree-sitter-gdscript.git"
                                          :ext "\\.gd\\'"))

  (add-to-list 'treesit-auto-recipe-list
               (make-treesit-auto-recipe
                :lang 'lua
                :ts-mode 'lua-ts-mode
                :remap 'lua-mode
                :url "https://github.com/tree-sitter-grammars/tree-sitter-lua"
                :abi14-revision "v0.3.0"
                :ext "\\.lua\\'"))

  (add-to-list 'treesit-auto-recipe-list
               (make-treesit-auto-recipe
                :lang 'javascript
                :ts-mode 'js-ts-mode
                :remap '(js-mode javascript-mode js2-mode)
                :url "https://github.com/tree-sitter/tree-sitter-javascript"
                :revision "master"
                :abi14-revision "v0.23.1"
                :source-dir "src"
                :ext "\\.js\\'"))

  (setq treesit-auto-langs '(c-sharp scala yaml gdscript json markdown dockerfile c lua))
  (treesit-auto-add-to-auto-mode-alist)
  (global-treesit-auto-mode 1))

(use-package typescript-ts-mode
  :mode ("\\.ts" . typescript-ts-mode)
  :config
  (jacob-defhookf typescript-ts-mode-hook
    (setq-local forward-sexp-function nil)
    (setq-local transpose-sexps-function nil)))

(use-package yaml-mode
  :mode ("\\.ya?ml\\'" . yaml-ts-mode))

(use-package yaml-pro
  :hook ((yaml-ts-mode-hook . yaml-pro-ts-mode)))

(use-package php-ts-mode
  :mode ("\\.php\\'" . php-ts-mode))

(use-package message
  :hook ((message-mode-hook . jacob-disable-auto-save-in-buffer))
  :custom ((message-send-mail-function 'smtpmail-send-it)))

(use-package nxml-mode
  :mode ("Directory.Packages.props" . nxml-mode))

(use-package avy
  :bind (("M-j" . avy-goto-char-timer)
         :map isearch-mode-map
         ("M-j" . avy-isearch))
  :functions (avy-action-mark
              avy-action-kill-stay
              avy-action-yank)
  :config
  (add-to-list 'avy-dispatch-alist (cons ?t #'avy-action-mark))
  (add-to-list 'avy-dispatch-alist (cons ?x #'avy-action-kill-stay))
  (add-to-list 'avy-dispatch-alist (cons ?v #'avy-action-yank))
  :custom ((avy-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l ?\;))))

(use-package jacob-avy
  :after (avy)
  :functions (jacob-avy-kill-line
              jacob-avy-copy-line
              jacob-avy-yank-line
              jacob-avy-embark)
  :config
  (add-to-list 'avy-dispatch-alist (cons ?X #'jacob-avy-kill-line))
  (add-to-list 'avy-dispatch-alist (cons ?C #'jacob-avy-copy-line))
  (add-to-list 'avy-dispatch-alist (cons ?V #'jacob-avy-yank-line))
  (add-to-list 'avy-dispatch-alist (cons ?\\ #'jacob-avy-embark)))

(use-package apheleia
  :defer t
  :init
  (keymap-set global-map "<menu-bar> <tools> <apheleia>" '("Format Buffer" . apheleia-format-buffer))
  :config
  (add-to-list 'apheleia-formatters '(csharpier "dotnet" "csharpier" "--write-stdout"))
  (add-to-list 'apheleia-formatters '(gdscript-formatter "gdscript-formatter"))
  (add-to-list 'apheleia-formatters '(scalafmt "scalafmt" "--stdin" "--non-interactive" "--quiet" "--stdout"))

  (add-to-list 'apheleia-mode-alist '(csharp-ts-mode . csharpier))
  (add-to-list 'apheleia-mode-alist '(gdscript-ts-mode . gdscript-formatter))
  (add-to-list 'apheleia-mode-alist '("\\.routes\\'" . play-routes))
  (add-to-list 'apheleia-mode-alist '(scala-mode . scalafmt))
  (add-to-list 'apheleia-mode-alist '(fennel-mode . lisp-indent))

  (add-to-list 'apheleia-skip-functions #'region-active-p)
  (add-to-list 'apheleia-skip-functions #'active-minibuffer-window))

(use-package jacob-apheleia
  :after apheleia
  :functions (jacob-apheleia-smerge-active-p jacob-apheleia-yas-active-p)
  :config
  (add-to-list 'apheleia-formatters '(play-routes . jacob-apheleia-format-play-routes-file))

  (add-to-list 'apheleia-skip-functions #'jacob-apheleia-yas-active-p)
  (add-to-list 'apheleia-skip-functions #'jacob-apheleia-smerge-active-p))

(use-package rainbow-mode
  :jacob-ensure-safely t
  :hook ((prog-mode-hook . rainbow-mode)))

(use-package eglot-booster
  :after eglot
  :when (executable-find "emacs-lsp-booster")
  :hook ((after-init-hook . eglot-booster-mode)))

(use-package dape
  :defer t
  :functions (dape-info)
  :config
  (setopt dape-info-hide-mode-line nil
          dape-buffer-window-arrangement 'right)

  (add-to-list 'dape-configs '(netcoredbg-attach-port
                               modes (csharp-mode csharp-ts-mode)
                               ensure dape-ensure-command
                               command "netcoredbg"
                               command-args ["--interpreter=vscode"]
                               :request "attach"
                               :cwd dape-cwd-fn
                               :program csharp-toolbox--select-dll
                               :stopAtEntry t
                               :processId
                               (lambda ()
                                 (let* ((collection
                                         (seq-map
                                          (lambda (pid)
                                            (cons (cdr (assoc 'args
                                                              (process-attributes pid)))
                                                  pid))
                                          (list-system-processes)))
                                        (selection (completing-read "process: "
                                                                    collection)))
                                   (cdr (assoc selection collection))))))

  (remove-hook 'dape-start-hook #'dape-info))

(use-package tex
  :commands (TeX-PDF-mode)
  :mode ("\\.tex\\'" . latex-mode)
  :config
  (add-hook 'LaTeX-mode-hook #'toggle-word-wrap 1)
  (add-hook 'LaTeX-mode-hook #'TeX-PDF-mode 1)
  :custom ((TeX-auto-save t)
           (TeX-parse-self t)
           (japanese-TeX-error-messages nil)))

(use-package vertico
  :if (and (not jacob-is-android)
           jacob-is-fast)
  :hook ((jacob-first-minibuffer-activation-hook . vertico-mode))
  :custom ((vertico-count 20)
           (vertico-resize t)))

(use-package warnings
  :defer t
  :custom ((warning-minimum-level :error)))

(use-package vertico-mouse
  :if (not jacob-is-android)
  :hook ((vertico-mode-hook . vertico-mouse-mode)))

(use-package ace-window
  :defer t
  :custom ((aw-keys '(?a ?s ?d ?f ?q ?w ?e ?r))
           (aw-minibuffer-flag t)
           (aw-scope 'frame)
           (aw-dispatch-when-more-than 3)))

(use-package marginalia
  :hook ((jacob-first-minibuffer-activation-hook . marginalia-mode)))

(use-package consult
  :bind (("C-x b" . consult-buffer)
         ("M-g g" . consult-goto-line)
         ("M-g i" . consult-imenu)
         ("M-s o" . consult-line)
         ("M-y" . consult-yank-from-kill-ring)
         ("M-g M-g" . consult-goto-line)
         :map project-prefix-map
         ("g" . consult-git-grep)
         :map minibuffer-local-map
         ("M-s" . consult-history)
         ("M-r" . consult-history))
  :config
  (add-to-list 'consult-buffer-sources 'jacob-consult-source-compile "APPEND")
  (add-to-list 'consult-buffer-sources 'jacob-consult-source-magit "APPEND")
  (setq completion-in-region-function 'consult-completion-in-region
        xref-show-xrefs-function 'consult-xref
        xref-show-definitions-function 'consult-xref
        consult-source-buffer (plist-put consult-source-buffer
                                         :state #'jacob-consult-buffer-state-no-tramp))
  :custom ((consult-preview-key (list :debounce 2 'any))))

(use-package embark
  :bind
  (("C-." . embark-act)
   ("C-;" . embark-dwim)
   :map minibuffer-local-map
   ("C-e" . embark-export)
   :map embark-general-map
   ("w" . nil)
   ("c" . embark-copy-as-kill)
   ("x" . kill-region)
   :map embark-flymake-map
   ("a" . eglot-code-actions)
   ("r" . eglot-rename))
  :config
  (setf (alist-get 'eglot-code-actions embark-target-injection-hooks) 'embark--ignore-target
        (alist-get 'eglot-rename embark-target-injection-hooks) 'embark--ignore-target))

(use-package embark-consult
  :defer t
  :after (:and embark consult))

(use-package cape
  :defer t
  :functions (cape-dabbrev)
  :init
  (add-to-list 'completion-at-point-functions #'cape-dabbrev))

(use-package expreg
  :bind (("C-c SPC" . expreg-expand)
         :repeat-map jacob-expreg-repeat-map
         ("SPC" . expreg-expand))
  :config
  (setq-default expreg-functions (remove 'expreg--subword expreg-functions)))

(use-package verb
  :defer t
  :functions (verb-json-get verb-stored-response)
  :config
  (defun jacob-verb-id (response-id)
    "Get the id property from the stored verb response pertaining to RESPONSE-ID."
    (verb-json-get (oref (verb-stored-response response-id) body) "id")))

(use-package sly
  :hook ((lisp-mode-hook . sly-mode))
  :functions (sly-setup sly-symbol-completion-mode)
  :config
  (sly-setup)

  (sly-symbol-completion-mode 0)

  (setopt sly-auto-start 'always
          inferior-lisp-program "sbcl"))

(use-package sly-overlay
  :after sly)

(use-package sly-macrostep
  :after sly)

;; (jacob-require sly-stepper "https://github.com/joaotavora/sly-stepper.git")

(use-package sly-quicklisp
  :after sly)

(use-package sql-indent
  :hook ((sql-mode-hook . sqlind-minor-mode)))

(use-package gptel
  :defer t
  :config
  (require 'gptel-integrations)
  (add-to-list 'gptel-prompt-prefix-alist '(org-mode . "** "))
  :custom ((gptel-confirm-tool-calls t)
           (gptel-default-mode #'org-mode)))

(use-package mcp
  :after gptel)

(use-package mcp-hub
  :defer t
  :config
  (add-to-list 'mcp-hub-servers '("elisp-dev"
                                  :command "~/.emacs.d/emacs-mcp-stdio.sh"
                                  :args ("--init-function=elisp-dev-mcp-enable"
                                         "--stop-function=elisp-dev-mcp-disable"
                                         "--server-id=elisp-dev-mcp"))))

(use-package elisp-dev-mcp
  :after gptel)

(require 'jacob-cecli)

(use-package gdscript-mode
  :hook ((gdscript-ts-mode-hook . apheleia-mode)
         (gdscript-ts-mode-hook . yas-minor-mode)
         (gdscript-ts-mode-hook . electric-indent-local-mode)
         (gdscript-ts-mode-hook . jacob-trim-quotes-mode)
         (gdscript-ts-mode-hook . indent-tabs-mode)))

(use-package eat
  :when (or jacob-is-linux jacob-is-mac)
  :hook ((eshell-mode-hook . eat-eshell-mode))
  :functions (eat-eshell-update-semi-char-mode-map)
  :config
  (add-to-list 'eat-eshell-semi-char-non-bound-keys [?\e ? ]) ; make M-SPC not bound in eat-eshell
  (eat-eshell-update-semi-char-mode-map) ; update the eat keymap
  )

(use-package exec-path-from-shell
  :if (or jacob-is-mac jacob-is-linux)
  :functions (exec-path-from-shell-initialize)
  :config
  (add-to-list 'exec-path-from-shell-variables "JAVA_HOME")
  (exec-path-from-shell-initialize))

(use-package pdf-tools
  :when jacob-is-linux
  :hook ((pdf-view-mode-hook . pdf-view-fit-page-to-window)
         (pdf-view-mode-hook . pdf-view-themed-minor-mode))
  :mode ("\\.[pP][dD][fF]\\'" . pdf-view-mode))

(use-package nov
  :mode ("\\.epub\\'" . nov-mode))

(use-package grep
  :defer t
  :config
  (when jacob-is-windows
    (setq find-program "C:/Program Files (x86)/GnuWin32/bin/find.exe"))
  :custom ((grep-use-headings t)))

(use-package wgrep
  :bind ( :map grep-mode-map
          ("e" . wgrep-change-to-wgrep-mode))
  :custom ((wgrep-auto-save-buffer t)))

(use-package dictionary
  :defer t
  :custom ((dictionary-server "localhost")))


;; personal functions

(define-minor-mode jacob-screen-sharing-mode
  "Minor mode for sharing screens."
  :global t
  :group 'jacob
  (if jacob-screen-sharing-mode
      (progn
        (global-display-line-numbers-mode 1))
    (global-display-line-numbers-mode 0)))

(defun jacob-ip-to-kill-ring ()
  "Copy v4 ip address to kill ring."
  (interactive)
  (kill-new (with-temp-buffer
              (shell-command "curl --silent -4 ifconfig.me" t)
              (buffer-string))))

(defun jacob-random-init ()
  "Go to a random place in init file."
  (interactive)
  (find-file user-init-file)
  (goto-char (random (point-max))))

(defun jacob-swap-visible-buffers ()
  "If two windows in current frame, swap their buffers.
Otherwise, display error message."
  (interactive)
  (if (length= (window-list) 2)
      (let* ((current-window (car (window-list)))
             (other-window (cadr (window-list)))
             (current-buffer (window-buffer current-window))
             (other-buffer (window-buffer other-window)))
        (set-window-buffer current-window other-buffer)
        (set-window-buffer other-window current-buffer)
        (other-window 1))
    (message "More/less than 2 windows in frame.")))

(defun jacob-update-config ()
  "Update your Emacs configuration with git."
  (interactive)
  (let ((default-directory (file-name-directory user-init-file)))
    (shell-command "git stash")
    (shell-command "git pull")
    (shell-command "git stash pop")))

(defun jacob-update-config-and-packages ()
  "Get latest config and update packages."
  (interactive)
  (jacob-update-config)
  (jacob-package-upgrade-all))

(defun jacob-bash-export-to-setenv ()
  "Convert the bash export statement at point to a `setenv' call."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (re-search-forward "export \\(.+\\)=\\(.+\\)")
    (replace-match "(setenv \"\\1\" \"\\2\")")))

(defun jacob-create-gnus-config ()
  "Append basic gnus config to your environment.el file."
  (interactive)
  (with-temp-buffer
    (insert "(setq user-full-name \"YOUR-NAME\"
      user-mail-address \"YOUR-EMAIL\"
      gnus-select-method '(nnnil nil)
      gnus-secondary-select-methods '((nnimap \"YOUR-IMAP-SERVER\")))")
    (write-region (point-min) (point-max) jacob-environment-file "append")))

(defun jacob-open-in-vscode ()
  "Open current file in vscode."
  (interactive)
  (let ((default-directory (project-root (project-current)))
        (file (buffer-file-name))
        (line (number-to-string (+ (line-number-at-pos (point)) 1)))
        (column (number-to-string (+ (current-column) 1))))
    (shell-command (concat "code . --reuse-window --goto \"" file "\":" line ":" column))))

(defun jacob-dvorak-to-qwerty (char)
  "Convert CHAR from dvorak to qwerty."
  (alist-get char '((?' . ?q)
                    (?, . ?w)
                    (?. . ?e)
                    (?p . ?r)
                    (?y . ?t)
                    (?f . ?y)
                    (?g . ?u)
                    (?c . ?i)
                    (?r . ?o)
                    (?l . ?p)
                    (?a . ?a)
                    (?o . ?s)
                    (?e . ?d)
                    (?u . ?f)
                    (?i . ?g)
                    (?d . ?h)
                    (?h . ?j)
                    (?t . ?k)
                    (?n . ?l)
                    (?s . ?\;)
                    (?\; . ?z)
                    (?q . ?x)
                    (?j . ?c)
                    (?k . ?v)
                    (?x . ?b)
                    (?b . ?n)
                    (?m . ?m)
                    (?w . ?,)
                    (?v . ?.)
                    (?z . ?/))))

(provide 'init)

;;; init.el ends here
