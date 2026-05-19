;;; init.el --- Jacob's main init file. -*-lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; `no-littering' needs to be loaded ASAP
(use-package no-littering)

;; constants and variables

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

(add-to-list 'load-path jacob-lisp-directory)
(add-to-list 'custom-theme-load-path jacob-lisp-directory)

(require 'jacob-init-helpers)
(require 'jacob-autoloads)

;; read custom file and environment file

(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file)

(when (file-exists-p jacob-environment-file)
  (ignore-errors (load-file jacob-environment-file)))

;; configure packages

(use-package jacob-editing-commands
  :demand
  :bind (("DEL" . jacob-delete-backwards)  ; `delete-backward-char'
         ("C-k" . jacob-kill-line)         ; `kill-line'
         ("C-a" . jacob-beginning-of-line) ; `beginning-of-line'
         ("C-e" . jacob-end-of-line)       ; `end-of-line'
         ))

(use-package jacob-modal-editing
  :config
  (require 'jacob-modal-editing-config)
  (jacob-modal-editing-mode 1))

(use-package use-package
  :config
  (when use-package-compute-statistics
    (add-hook 'after-init-hook #'use-package-report)))

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

  (easy-menu-define jacob-ui-menu global-map
    "UI menu"
    '("JUI"
      ["Theme" consult-theme t]
      ["Line Numbers" global-display-line-numbers-mode
       :style toggle
       :selected global-display-line-numbers-mode]))

  (easy-menu-define jacob-tools-menu global-map
    "Tools"
    '("JTools"
      ["Magit" magit t]
      ["Agenda" org-agenda t]
      ["Calendar" calendar t]
      ["Describe" consult-symbol t]
      ["Cecli" jacob-cecli t]
      ["Gptel" gptel t]))

  (easy-menu-define jacob-move-menu global-map
    "Movement menu"
    '("JMove"
      ["Buffer" consult-buffer t]
      ["Project File" project-find-file t]
      ["Switch Project" jacob-dired-in-other-project t]
      ["File" find-file t]
      ["Imenu" consult-imenu t]
      ["Dired" dired-jump t]))

  (easy-menu-define jacob-menu global-map
    "misc menu"
    '("Jacob"
      ["M-x" execute-extended-command t]
      ["Restart" restart-emacs t]
      ["Bookmark" bookmark-jump t])))

(use-package tool-bar
  :config
  (when jacob-is-android
    (modifier-bar-mode 1))
  :custom
  (tool-bar-button-margin (if jacob-is-android 40 4))
  (tool-bar-position (if jacob-is-android 'bottom 'top))
  (tool-bar-style 'image))

;; custom hooks

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

  :custom
  ;; c source code
  (truncate-lines (cond (jacob-is-android t)
                        (t nil)))
  (echo-keystrokes (cond (jacob-is-android 1)
                         (t 0.01)))
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
  (sentence-end-double-space nil)
  :bind (("C-M-k" . jacob-kill-sexp)    ; `kill-sexp'
         :map mode-line-buffer-identification-keymap
         ("<mode-line> <mouse-2>" . ibuffer)))

(use-package on)                    ; load `on-first-input-hook', etc.

(use-package which-key
  :blackout
  :hook (on-first-input-hook . which-key-mode)
  :custom (which-key-idle-delay (cond (jacob-is-android 1)
                                      (t 0.01))))

(use-package mouse
  :hook (on-first-input-hook . context-menu-mode))

(use-package mwheel
  :custom ((mouse-wheel-scroll-amount '(10 ((shift) . hscroll)
                                           ((meta))
                                           ((control) . text-scale)))))

(use-package tooltip
  :custom (tooltip-delay (cond (jacob-is-android 0.7)
                               (t 0.1))))

(use-package files
  :hook (on-first-file-hook . auto-save-visited-mode))

(use-package files-x
  :config
  (connection-local-set-profile-variables
   'remote-direct-async-process
   '((tramp-direct-async-process . t)))

  (connection-local-set-profiles
   '(:application tramp :protocol "scp")
   'remote-direct-async-process))

(use-package autorevert
  :blackout
  :hook (on-first-file-hook . global-auto-revert-mode))

(use-package window
  :bind ( :repeat-map jacob-window-repeat-map
          ("b" . consult-buffer)
          ("o" . other-window))
  :custom
  (display-buffer-alist '(((major-mode . sql-interactive-mode)
                           (display-buffer-reuse-mode-window display-buffer-same-window))
                          ((major-mode . prodigy-mode)
                           (display-buffer-reuse-mode-window display-buffer-same-window))
                          ((major-mode . magit-status-mode)
                           (display-buffer-reuse-mode-window display-buffer-same-window))
                          ((or (derived-mode . slack-mode)
                               (derived-mode . lui-mode))
                           (display-buffer-in-side-window)
                           (side . right)))))

(defvar-keymap jacob-recenter-repeat-map
  :repeat t
  "p" #'recenter-top-bottom)

(use-package frame
  :config
  (blink-cursor-mode 0)
  :custom (blink-cursor-blinks 0)     ; make cursor blink forever
  :bind ("C-z" . nil)                 ; `suspend-frame'
  )

(use-package novice
  :defer t
  :config
  (setq disabled-command-function nil))

(use-package recentf
  :hook (on-first-input-hook . recentf-mode))

(use-package savehist
  :hook (jacob-first-minibuffer-activation-hook . savehist-mode)
  :custom
  (savehist-save-minibuffer-history t))

(use-package saveplace
  :hook (on-first-file-hook . save-place-mode)
  :custom
  (save-place-forget-unreadable-files t))

(use-package generic-x           ; support for files like `/etc/fstab'
  :defer t)

(use-package conf-mode
  :defer t
  :bind ( :map conf-mode-map
          ("C-c SPC" . nil)))

(use-package simple
  :hook (on-init-ui-hook . column-number-mode)
  :hook (on-init-ui-hook . line-number-mode)
  :bind (("C-x u" . nil)                ; `undo'
         )
  :config
  (put 'set-goal-column 'disabled nil))

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
          ("<left>" . isearch-repeat-backward)))

(use-package bookmark
  :defer t
  :config
  (require 'jacob-bookmark))

(use-package flymake
  :bind
  ("M-n" . flymake-goto-next-error)
  ("M-p" . flymake-goto-prev-error))

(use-package yasnippet
  :defer t
  :blackout "yas"
  :bind ( :map yas-minor-mode-map
          ("C-c y n" . yas-new-snippet)
          ("C-c y v" . yas-visit-snippet-file)
          ("C-c y i" . yas-insert-snippet))
  :config
  (require 'jacob-yasnippet)
  (yas-reload-all)
  (jacob-defhookf snippet-mode-hook
    (setq-local auto-save-visited-mode nil))
  :custom
  (yas-new-snippet-default "# -*- mode: snippet -*-
# key: $1
# --
$0`(yas-escape-text yas-selected-text)`"))

(use-package minibuffer
  :config
  (keymap-set minibuffer-local-completion-map "SPC" #'self-insert-command)
  (with-eval-after-load "eglot"
    (setq completion-category-defaults (seq-filter (lambda (category)
                                                     "Remove eglot-capf from `completion-category-defaults'."
                                                     (not (eq 'eglot-capf (car category))))
                                                   completion-category-defaults)))
  :custom
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package mb-depth
  :hook (jacob-first-minibuffer-activation-hook . minibuffer-depth-indicate-mode))

(use-package man
  :defer
  :custom
  (Man-notify-method 'pushy))

(use-package help-fns
  :defer t
  :config
  (require 'jacob-help-fns)
  (put 'help-fns-edit-variable 'disabled nil))

(use-package subword
  :blackout
  :hook (on-first-input-hook . global-subword-mode))

(use-package paren
  :hook (on-first-input-hook . show-paren-mode))

(use-package elec-pair
  :hook (on-first-input-hook . electric-pair-mode))

(use-package puni
  :bind (("M-d" . puni-forward-kill-word) ; `kill-word'
         ("M-DEL" . puni-backward-kill-word) ; `backward-kill-word'
         ("C-M-f" . puni-forward-sexp-or-up-list) ; `forward-sexp'
         ("C-M-b" . puni-backward-sexp-or-up-list) ; `backward-sexp'
         )
  :config
  (require 'jacob-puni))

(use-package delsel
  :hook (on-first-input-hook . delete-selection-mode))

(use-package repeat
  :hook (on-first-input-hook . repeat-mode))

(use-package magit
  :bind ( :map project-prefix-map
          ("v" . magit-project-status)))

(use-package magit-process
  :defer t
  :config
  (setq magit-tramp-pipe-stty-settings 'pty))

(use-package autoinsert
  :hook (on-first-file-hook . auto-insert-mode)
  :config
  (jacob-define-auto-insert "\\.el$" ["template.el" checkdoc elisp-enable-lexical-binding])
  (jacob-define-auto-insert "\\.scala$" ["template.scala" jacob-autoinsert-yas-expand])
  (jacob-define-auto-insert "\\.cs$" ["template.cs" jacob-autoinsert-yas-expand])
  (jacob-define-auto-insert "Controller\\.cs$" ["controllerTemplate.cs" jacob-autoinsert-yas-expand]))

(use-package tramp
  :defer t
  :config
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path)
  (setq tramp-archive-enabled nil) ; lots of problems. for now, disable it!
  )

(use-package dumb-jump
  :defer t
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
  :config
  (require 'jacob-eglot)

  (add-to-list 'eglot--lsp-interface-alist
               '(RenameFile (:kind :oldUri :newUri)) ())

  (advice-add #'eglot--apply-workspace-edit :override #'jacob-eglot--apply-workspace-edit)

  (jacob-defhookf eglot-managed-mode-hook
    (eglot-inlay-hints-mode 0)
    (eglot-semantic-tokens-mode 0)
    (setq-local xref-backend-functions '(eglot-xref-backend dumb-jump-xref-activate t)))

  (setq-default eglot-workspace-configuration '(:metals ( :inlayHints ( :implicitArguments (:enable t)
                                                                        :implicitConversions (:enable t))
                                                          :autoImportBuilds "all"
                                                          :targetBuildTool "sbt"
                                                          :defaultBspToBuildTool t
                                                          :enableBestEffort t)))

  (advice-add 'eglot-xref-backend :override #'jacob-eglot-xref-backend)

  (advice-add 'eglot-code-actions :after #'jacob-remove-ret-character-from-buffer)
  (advice-add 'eglot-rename :after #'jacob-remove-ret-character-from-buffer)

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
          eglot-stay-out-of '(imenu)
          eglot-code-action-indications '(eldoc-hint mode-line)))

(require 'jacob-csharp-mode)

(require 'jacob-sharper)

(use-package csproj-mode
  :mode ("\\.csproj\\'" . csproj-mode))

(use-package sln-mode
  :mode ("\\.sln\\'" . sln-mode))

(use-package fsharp-mode
  :defer t
  :mode ("\\.fs\\'" . fsharp-mode)
  :config
  (remove-hook 'project-find-functions #'fsharp-mode-project-root)
  (setopt compilation-error-regexp-alist (remq 'fsharp compilation-error-regexp-alist)))

(use-package scala-ts-mode
  :mode ("\\.scala\\'" . scala-ts-mode)
  :hook ((scala-ts-mode-hook . apheleia-mode)
         (scala-ts-mode-hook . yas-minor-mode)
         (scala-ts-mode-hook . electric-indent-local-mode)
         (scala-ts-mode-hook . jacob-trim-quotes-mode)
         (scala-ts-mode-hook . eglot-ensure)
         (scala-ts-mode-hook . stripspace-local-mode)
         (scala-ts-mode-hook . jacob-scala-font-lock-setup))
  :config
  (require 'jacob-scala)
  :bind ( :map scala-ts-mode-map
          ("$" . jacob-scala-dollar)))

(use-package sbt-mode
  :defer t
  :config
  (advice-add #'sbt:initialize-for-compilation-mode :override #'ignore)
  (add-hook 'sbt-mode-hook #'compilation-shell-minor-mode))

(use-package web-mode
  :mode ("\\.scala\\.html\\'" . web-mode)
  :custom (web-mode-engines-alist
           '(("play" . "\\.scala\\.html\\'")))
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

(require 'jacob-dired)

(use-package nerd-icons-dired
  :blackout
  :when (display-graphic-p)
  :hook (dired-mode-hook . nerd-icons-dired-mode))

(use-package nerd-icons-mode-line
  :when (display-graphic-p)
  :hook (on-first-file-hook . nerd-icons-mode-line-global-mode))

(use-package nerd-icons-completion
  :when (display-graphic-p)
  :hook (prog-mode-hook . nerd-icons-completion-mode))

(use-package nerd-icons-grep
  :when (display-graphic-p)
  :hook (grep-mode-hook . nerd-icons-grep-mode))

(use-package nerd-icons-xref
  :when (display-graphic-p)
  :hook (xref--xref-buffer-mode-hook . nerd-icons-xref-mode))

(use-package nerd-icons-ibuffer
  :when (display-graphic-p)
  :hook (ibuffer-mode-hook . nerd-icons-ibuffer-mode))

(use-package esh-mode
  :defer t
  :config
  (require 'jacob-eshell)

  (when jacob-is-windows
    (advice-add 'eshell-interrupt-process
                :after
                #'jacob-eshell-windows-confirm-terminate-batch-job)))

(use-package eldoc
  :hook (prog-mode-hook . global-eldoc-mode)
  :blackout
  :config
  (setopt eldoc-documentation-strategy 'eldoc-documentation-compose))

(use-package consult-git-log-grep
  :defer t
  :init
  (keymap-set project-prefix-map "l" #'consult-git-log-grep))

(use-package prodigy
  :defer t
  :init
  (keymap-set project-prefix-map "L" #'prodigy)
  :config
  (setopt prodigy-kill-process-buffer-on-stop t)
  
  (prodigy-define-tag
    :name 'asp.net
    :stop-signal 'kill
    :on-output (lambda (&rest args)
                 (let ((output (plist-get args :output))
                       (service (plist-get args :service)))
                   (when (string-match-p "Hosting started *$" output)
                     (prodigy-set-status service 'ready)))))

  (add-hook 'prodigy-view-mode-hook #'compilation-minor-mode))

(use-package hi-lock
  :defer t
  :blackout)

(use-package hl-todo
  :hook (after-init-hook . global-hl-todo-mode))

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
  :defer t
  :hook ((emacs-lisp-mode-hook . apheleia-mode)
         (emacs-lisp-mode-hook . jacob-font-lock-programming-setup)
         (emacs-lisp-mode-hook . yas-minor-mode)
         (emacs-lisp-mode-hook . electric-indent-local-mode)
         (emacs-lisp-mode-hook . flymake-mode))
  :config
  (defun jacob-move-past-close-and-reindent ()
    "Advice for `move-past-close-and-reindent'."
    (when (bolp)
      (delete-blank-lines)))

  (advice-add #'move-past-close-and-reindent :after #'jacob-move-past-close-and-reindent)

  (jacob-defhookf emacs-lisp-mode-hook
    (setq-local yas-key-syntaxes '("w_")))

  (defun jacob-eval-print-last-sexp ()
    "Run `eval-print-last-sexp', indent the result."
    (interactive)
    (save-excursion
      (eval-print-last-sexp 0))
    (save-excursion
      (forward-line)
      (indent-pp-sexp t)))

  (keymap-set lisp-interaction-mode-map "C-j" #'jacob-eval-print-last-sexp)

  (setopt elisp-flymake-byte-compile-load-path load-path)

  (font-lock-add-keywords 'emacs-lisp-mode
                          '(("(use-package \\([[:word:]-]+\\)" 1 'font-lock-function-name-face))))

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
  :hook (org-mode-hook . yas-minor-mode)
  :config
  (require 'jacob-org)
  (add-hook 'org-babel-post-tangle-hook 'jacob-org-babel-tangle-delete-whitespace)

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

  (org-link-set-parameters "jira"
                           :follow #'jacob-org-jira-follow)

  (org-link-set-parameters "project"
                           :follow #'jacob-org-project-follow))

(use-package org-agenda
  :commands (org-agenda org-capture)
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
    (setq-local tool-bar-map org-agenda-tool-bar-map)
    (hl-line-mode 1))
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

(use-package ox-latex
  :after org
  :config
  (setopt org-latex-pdf-process (list "latexmk -pdf %f -shell-escape")) ; probably requires texlive
  )

;; (require 'ox-extra)

;; (ox-extras-activate '(latex-header-blocks ignore-headlines))

(use-package denote
  :defer t)

(use-package action-lock
  :defer t
  :blackout)

(use-package pulse
  :defer t
  :init
  (require 'jacob-pulse)

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
  :hook (after-init-hook . server-start))

(use-package smerge-mode
  :defer t
  :config
  (defvar-keymap jacob-smerge-repeat-map
    :repeat t
    "l" #'smerge-next
    "j" #'smerge-prev
    "i" #'smerge-keep-upper
    "k" #'smerge-keep-lower
    "SPC" #'smerge-keep-all))

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
  :defer t
  :commands (winner-undo winner-redo)
  :hook (on-first-input-hook . winner-mode))

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
    (setq compilation-error-regexp-alist (remq re compilation-error-regexp-alist)))

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

  (setopt compilation-always-kill t
          compilation-scroll-output t
          compilation-ask-about-save nil)

  (with-eval-after-load 'tramp
    (remove-hook 'compilation-mode-hook #'tramp-compile-disable-ssh-controlmaster-options))

  (with-eval-after-load "jacob-modal-editing"
    ;; Hack to prevent compilation mode keys from overriding modal
    ;; editing keys. revisit if the overriding problem appears in more
    ;; places.
    (jacob-modal-editing-ensure-priority)))

(use-package winnow
  :hook (compilation-mode-hook . winnow-mode))

(require 'jacob-sql)

(use-package treesit
  :defer t
  :custom
  (treesit-font-lock-level 4))

(use-package treesit-auto
  :config
  (add-to-list 'treesit-auto-recipe-list (make-treesit-auto-recipe
                                          :lang 'gdscript
                                          :ts-mode 'gdscript-ts-mode
                                          :remap 'gdscript-mode
                                          :url "https://github.com/PrestonKnopp/tree-sitter-gdscript.git"
                                          :ext "\\.gd\\'"))

  (setq treesit-auto-langs '(c-sharp scala yaml gdscript json markdown dockerfile))
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
  :hook (yaml-ts-mode-hook . yaml-pro-ts-mode))

(use-package php-ts-mode
  :mode ("\\.php\\'" . php-ts-mode))

(use-package message
  :defer t
  :config
  (jacob-defhookf message-mode-hook
    (setq-local auto-save-visited-mode nil)))

(use-package nxml-mode
  :mode ("Directory.Packages.props" . nxml-mode))

(use-package avy
  :bind (("M-j" . avy-goto-char-timer)
         :map isearch-mode-map
         ("M-j" . avy-isearch))
  :config
  (require 'jacob-avy))

(use-package apheleia
  :defer t
  :blackout " ⚘"
  :config
  (require 'jacob-apheleia)

  (keymap-set global-map "<menu-bar> <tools> <apheleia>" '("Format Buffer" . apheleia-format-buffer))

  (add-to-list 'apheleia-formatters '(csharpier "dotnet" "csharpier" "--write-stdout"))
  (add-to-list 'apheleia-formatters '(gdscript-formatter "gdscript-formatter"))
  (add-to-list 'apheleia-formatters '(play-routes . jacob-apheleia-format-play-routes-file))
  (add-to-list 'apheleia-formatters '(scalafmt "scalafmt" "--stdin" "--non-interactive" "--quiet" "--stdout"))

  (add-to-list 'apheleia-mode-alist '(csharp-ts-mode . csharpier))
  (add-to-list 'apheleia-mode-alist '(gdscript-ts-mode . gdscript-formatter))
  (add-to-list 'apheleia-mode-alist '("\\.routes\\'" . play-routes))
  (add-to-list 'apheleia-mode-alist '(scala-ts-mode . scalafmt))
  (add-to-list 'apheleia-mode-alist '(fennel-mode . lisp-indent))

  (add-to-list 'apheleia-skip-functions #'region-active-p)
  (add-to-list 'apheleia-skip-functions #'active-minibuffer-window)
  (add-to-list 'apheleia-skip-functions #'jacob-apheleia-yas-active-p)
  (add-to-list 'apheleia-skip-functions #'jacob-apheleia-smerge-active-p))

(use-package rainbow-mode
  :blackout
  :hook (on-first-file-hook . rainbow-mode))

(use-package eglot-booster
  :after eglot
  :when (executable-find "emacs-lsp-booster")
  :config
  (eglot-booster-mode 1))

(use-package dape
  :defer t
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

(require 'jacob-ace-window)

(use-package tex
  :commands (TeX-PDF-mode)
  :mode ("\\.tex\\'" . latex-mode)
  :config
  (add-hook 'LaTeX-mode-hook #'toggle-word-wrap 1)
  (add-hook 'LaTeX-mode-hook #'TeX-PDF-mode 1)
  :custom ((TeX-auto-save t)
           (TeX-parse-self t)
           (japanese-TeX-error-messages nil)))

(use-package mct
  :if jacob-is-android
  :hook (jacob-first-minibuffer-activation-hook . mct-mode))

(use-package vertico
  :if (not jacob-is-android)
  :hook (jacob-first-minibuffer-activation-hook . vertico-mode)
  :custom (vertico-resize t))

(use-package vertico-mouse
  :if (not jacob-is-android)
  :hook (vertico-mode-hook . vertico-mouse-mode))

(use-package ace-window
  :defer t
  :custom
  (aw-keys '(?a ?s ?d ?f ?q ?w ?e ?r))
  (aw-minibuffer-flag t)
  (aw-scope 'frame)
  (aw-dispatch-when-more-than 3))

(use-package marginalia
  :hook (jacob-first-minibuffer-activation-hook . marginalia-mode))

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
  (setq completion-in-region-function 'consult-completion-in-region
        xref-show-xrefs-function 'consult-xref
        xref-show-definitions-function 'consult-xref
        consult-source-buffer (plist-put consult-source-buffer
                                         :state #'jacob-consult-buffer-state-no-tramp)))

(use-package consult-symbol
  :config
  (require 'jacob-consult-symbol)
  (advice-add #'consult-symbol--default-action :override #'jacob-consult-symbol--default-action))

(use-package embark
  :defer t
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
  :init
  (add-to-list 'completion-at-point-functions #'cape-dabbrev))

(use-package expreg
  :bind (("C-c SPC" . expreg-expand)
         :repeat-map jacob-expreg-repeat-map
         ("SPC" . expreg-expand))
  :config
  (setq-default expreg-functions (remq 'expreg--subword expreg-functions)))

(use-package verb
  :defer t
  :config
  (defun jacob-verb-id (response-id)
    "Get the id property from the stored verb response pertaining to RESPONSE-ID."
    (verb-json-get (oref (verb-stored-response response-id) body) "id")))

(use-package sly
  :hook (lisp-mode-hook . sly-mode)
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
  :hook (sql-mode-hook . sqlind-minor-mode))

(use-package gptel
  :defer t
  :config
  (require 'gptel-integrations)
  :custom
  (gptel-default-mode #'org-mode)
  (gptel-confirm-tool-calls t))

(use-package mcp
  :after gptel)

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
  :init
  (add-hook 'eshell-mode-hook #'eat-eshell-mode))

(use-package exec-path-from-shell
  :if (or jacob-is-mac jacob-is-linux)
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
    (setq find-program "C:/Program Files (x86)/GnuWin32/bin/find.exe")))

(use-package wgrep
  :bind ( :map grep-mode-map
          ("e" . wgrep-change-to-wgrep-mode)))

(use-package dictionary
  :custom
  (dictionary-server "localhost"))


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
  (let ((default-directory "~/.emacs.d/"))
    (shell-command "git stash")
    (shell-command "git pull")
    ;; (shell-command "git push")
    (shell-command "git stash pop")))

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

(provide 'init)

;;; init.el ends here
