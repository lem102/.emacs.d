;;; init.el --- Jacob's main init file. -*-lexical-binding: t; apheleia-inhibit: nil; -*-
;;; Commentary:
;;; Code:

;; constants and variables

(defconst jacob-lisp-directory (file-name-concat (file-name-directory user-init-file)
                                                 "lisp"))

(defconst jacob-environment-file (file-name-concat (file-name-directory user-init-file)
                                                   "environment.el"))

(defvar jacob-font-size 11
  "Font size to use.")

(defvar jacob-theme 'modus-vivendi
  "Theme to use.")

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

;; read custom file and environment file

(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file)

(when (file-exists-p jacob-environment-file)
  (ignore-errors (load-file jacob-environment-file)))

;; configure packages

(require 'jacob-ryo-modal)

(use-package use-package
  :config
  (when use-package-compute-statistics
    (add-hook 'after-init-hook #'use-package-report)))

(use-package no-littering)

(use-package on)

(use-package menu-bar
  :config
  (menu-bar-mode 1)

  (keymap-global-set "<menu-bar> <jacob>"
                     (cons "jacob"
                           (let ((keymap (make-sparse-keymap)))
                             (keymap-set keymap
                                         "<mx>"
                                         (cons "M-x"
                                               #'execute-extended-command))

                             (keymap-set keymap
                                         "<restart>"
                                         (cons "restart"
                                               #'restart-emacs))

                             (keymap-set keymap
                                         "<bookmark-jump>"
                                         (cons "bookmark"
                                               #'bookmark-jump))

                             (keymap-set keymap
                                         "<magit>"
                                         (cons "magit"
                                               #'magit))


                             (keymap-set keymap "<org-agenda>" (cons "org-agenda" #'org-agenda))

                             keymap))))

(use-package tool-bar
  :config
  (tool-bar-mode (if jacob-is-android 1 0))
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
    (advice-remove #'completing-read #'jacob-run-first-minibuffer-activation-hook)))

(advice-add #'completing-read :before #'jacob-run-first-minibuffer-activation-hook)

(use-package emacs
  :config
  ;; enable emoji fonts
  (set-fontset-font t
                    'emoji
                    (seq-find (lambda (font)
                                (member font (font-family-list)))
                              '("Symbola"
                                "Segoe UI Emoji"
                                "Noto Emoji"
                                "Noto Color Emoji"
                                "Apple Color Emoji")))

  (add-to-list 'default-frame-alist
               `(font . ,(format "%s-%s"
                                 (cdr (assoc-string system-type
                                                    '(("windows-nt" . "Consolas")
                                                      ("darwin" . "Menlo")
                                                      ("gnu/linux" . "DejaVu Sans Mono")
                                                      ("android" . "Droid Sans Mono"))))
                                 jacob-font-size)))

  ;; mule-cmds.el
  (prefer-coding-system 'utf-8)

  :custom
  ;; c code
  (scroll-conservatively 101)
  (tab-width 4) ; set default tab char's display width to 4 spaces
  (truncate-lines (cond (jacob-is-android t)
                        (t nil)))
  (delete-by-moving-to-trash t)
  (read-process-output-max (* 1024 1024))
  (frame-resize-pixelwise t)
  (create-lockfiles nil)
  (history-length 1000)
  (history-delete-duplicates t)
  (use-dialog-box nil)
  (use-short-answers t)
  (ring-bell-function 'ignore)
  (truncate-partial-width-windows nil)
  (enable-recursive-minibuffers t)
  (completion-ignore-case t)
  (kill-buffer-query-functions (delq 'process-kill-buffer-query-function
                                     kill-buffer-query-functions))
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
  (initial-scratch-message (format ";; %s\n\n"
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
  (insert-pair-alist (append insert-pair-alist
                             '((?k ?\( ?\))
                               (?l ?\[ ?\])
                               (?j ?\{ ?\})
                               (?u ?\" ?\")
                               (?i ?\' ?\')
                               (?h ?\< ?\>))))
  ;; bindings.el
  (mode-line-percent-position nil)
  ;; paragraphs.el
  (sentence-end-double-space nil)
  ;; indent.el
  ;; make tab key call indent command or insert tab character, depending on cursor position
  (tab-always-indent 'complete)
  :bind ( :map mode-line-buffer-identification-keymap
          ("<mode-line> <mouse-2>" . ibuffer)))

(use-package blackout)

(use-package which-key
  :blackout
  :hook (on-first-input-hook . which-key-mode)
  :custom (which-key-idle-delay (cond (jacob-is-android 1)
                                      (t 0.01))))

(use-package mouse
  :config
  (context-menu-mode 1))

(use-package mwheel
  :custom ((mouse-wheel-scroll-amount '(10 ((shift) . hscroll)
                                           ((meta))
                                           ((control) . text-scale)))))

(use-package tooltip
  :custom (tooltip-delay (cond (jacob-is-android 0.7)
                               (t 0.1))))

(use-package files
  :config
  (auto-save-visited-mode 1)
  :bind ("C-x C-c" . nil) ; `save-buffers-kill-terminal'
  :custom ((auto-save-default nil)
           (make-backup-files nil)
           (backup-by-copying t)
           (auto-save-visited-interval 2)   ; save file after two seconds
           (confirm-kill-processes nil)))

(use-package autorevert
  :blackout
  :hook (on-first-file-hook . global-auto-revert-mode))

(use-package window
  :custom
  (switch-to-buffer-obey-display-actions t)
  (display-buffer-alist '(((major-mode . sql-interactive-mode)
                           (display-buffer-reuse-mode-window display-buffer-same-window))
                          ((major-mode . prodigy-mode)
                           (display-buffer-reuse-mode-window display-buffer-same-window))
                          ((major-mode . magit-status-mode)
                           (display-buffer-reuse-mode-window display-buffer-same-window))
                          ((or (derived-mode . slack-mode)
                               (derived-mode . lui-mode))
                           (display-buffer-in-side-window)
                           (side . right))))

  ;; experimental display buffer alist changes
  ;; (display-buffer-alist '(((major-mode . magit-diff-mode)
  ;;                          nil)
  ;;                         ("\\`.*\\'" . (display-buffer-same-window display-buffer-full-frame))))
  (split-height-threshold nil))

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
  :custom (disabled-command-function nil))

(use-package recentf
  :hook (after-init-hook . recentf-mode)
  :custom
  (recentf-max-saved-items 300))

(use-package savehist
  :hook (jacob-first-minibuffer-activation-hook . savehist-mode)
  :custom
  (savehist-save-minibuffer-history t))

(use-package saveplace
  :hook (on-first-file-hook . save-place-mode)
  :custom
  (save-place-forget-unreadable-files t))

(use-package modus-themes
  :defer t
  :custom
  (modus-themes-to-toggle '(modus-operandi-tinted modus-vivendi-tinted)))

(use-package generic-x           ; support for files like `/etc/fstab'
  :defer t)

(use-package simple
  :config
  (column-number-mode 1)
  (line-number-mode 1)
  :custom
  (indent-tabs-mode nil)                ; use spaces to indent
  (save-interprogram-paste-before-kill t)
  (read-extended-command-predicate command-completion-default-include-p)
  (kill-do-not-save-duplicates t)
  :bind ("C-x u" . nil)                 ; `undo'
  )

(use-package bookmark
  :defer t
  :config
  (defun jacob-bookmark-command (bookmark)
    "Launch the command stored in bookmark.

Intended for running applications."
    (let ((command (bookmark-get-filename bookmark)))
      (start-process-shell-command command nil command)))

  (defun jacob-bookmark-firefox (bookmark)
    "Open BOOKMARK in firefox."
    (let ((command (concat "firefox-esr "
                           (bookmark-get-filename bookmark))))
      (start-process-shell-command command
                                   nil
                                   command)))

  (defun jacob-bookmark-chrome (bookmark)
    "Open BOOKMARK in chrome."
    (let ((command (concat "google-chrome "
                           (bookmark-get-filename bookmark))))
      (start-process-shell-command command
                                   nil
                                   command))))

(use-package flymake
  :defer t
  :init
  (keymap-global-set "M-n" #'flymake-goto-next-error)
  (keymap-global-set "M-p" #'flymake-goto-prev-error))

(defun jacob-end-of-line ()
  "Go to content end, line end, forward paragraph."
  (interactive)
  (if (eolp)
      (forward-paragraph)
    (let ((content-end (save-excursion
                         (when (comment-search-forward (line-end-position) "NOERROR")
                           (goto-char (match-beginning 0))
                           (skip-syntax-backward " <" (line-beginning-position))
                           (unless (= (point) (line-beginning-position))
                             (point))))))
      (if (or (null content-end)
              (= content-end (point)))
          (move-end-of-line 1)
        (goto-char content-end)))))

(defun jacob-beginning-of-line ()
  "Go to indentation, line start, backward paragraph."
  (interactive)
  (cond ((bolp)
         (backward-paragraph))
        ((= (save-excursion
              (back-to-indentation)
              (point))
            (point))
         (move-beginning-of-line 1))
        (t
         (back-to-indentation))))

(defvar-local jacob-backspace-function nil
  "Called by `jacob-backspace' if non-nil.")

(defun jacob-backspace ()
  "DWIM backspace command.

  If character to the left is a pair character as determined by
  `insert-pair-alist', kill from the pair to its match. If the prefix
  argument is provided, only delete the pair characters.

  If the character to the left of the cursor is whitespace, delete all
  the whitespace backward from point to the first non whitespace
  character."
  (interactive)
  (undo-boundary)
  (if (region-active-p)
      (delete-active-region)
    (push-mark)
    (when (= 1 (point))
      (user-error "Beginning of buffer"))
    (let ((char-class (char-syntax (char-before)))
          (delete-function (if current-prefix-arg
                               #'delete-pair
                             #'kill-sexp)))
      (unless (ignore-errors
                (funcall jacob-backspace-function delete-function))
        (cond ((= ?\" char-class)     ; string
               (if (nth 3 (syntax-ppss))
                   (progn
                     (backward-char)
                     (save-excursion
                       (forward-sexp)
                       (push-mark)))
                 (backward-sexp))
               (funcall delete-function))
              ((= ?\( char-class)     ; delete from start of pair
               (backward-char)
               (save-excursion
                 (forward-sexp)
                 (push-mark))
               (funcall delete-function))
              ((= ?\) char-class)     ; delete from end of pair
               (backward-sexp)
               (funcall delete-function))
              (t                      ; delete character
               (backward-delete-char-untabify 1)))))))

(require 'jacob-yasnippet)

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

(use-package ibuffer
  :defer t
  :init
  (with-eval-after-load "ryo-modal"
    (ryo-modal-key "SPC i d" #'ibuffer))
  :config
  (with-eval-after-load "ryo-modal"
    (ryo-modal-major-mode-keys 'ibuffer-mode
                               ("q" quit-window)
                               ("e" ibuffer-mark-forward)
                               ("r" ibuffer-unmark-forward)
                               ("g" ibuffer-update))))

(use-package replace
  :defer t
  :config
  (with-eval-after-load "ryo-modal"
    (ryo-modal-major-mode-keys 'occur-mode
                               ("q" quit-window)
                               ("i" occur-prev)
                               ("k" occur-next))))


(use-package info
  :defer t
  :config
  (with-eval-after-load "ryo-modal"
    (ryo-modal-major-mode-keys 'Info-mode
                               ("q" quit-window)
                               ("r" Info-scroll-up)
                               ("e" Info-scroll-down)
                               ("w" Info-up)
                               ("g" Info-menu))))

(use-package man
  :defer
  :custom
  (Man-notify-method 'pushy)
  :config
  (with-eval-after-load "ryo-modal"
    (ryo-modal-major-mode-keys 'Man-mode
                               ("q" quit-window))))

(use-package diff-mode
  :defer t
  :config
  (with-eval-after-load "ryo-modal"
    (ryo-modal-major-mode-keys 'diff-mode
                               ("q" quit-window)
                               ("e" diff-hunk-prev)
                               ("r" diff-hunk-next)
                               ("x" diff-hunk-kill)
                               ("g" revert-buffer))))

(use-package help
  :defer t
  :custom
  (help-window-select t)
  (help-enable-variable-value-editing t))

(use-package help-fns
  :defer t
  :init
  (defun jacob-help-edit ()
    "Edit variable in current help buffer."
    (interactive)
    (unless (equal major-mode 'help-mode)
      (message "not in help buffer"))
    (save-excursion
      (goto-char (point-min))
      (if (search-forward "Its value is " nil "NOERROR")
          (help-fns-edit-variable)
        (message "cannot find editable variable"))))
  
  (with-eval-after-load "ryo-modal"
    (ryo-modal-major-mode-keys 'help-mode 
                               ("w" jacob-help-edit))))

(use-package help-mode
  :defer t
  :config
  (with-eval-after-load "ryo-modal"
    (ryo-modal-major-mode-keys 'help-mode
                               ("s" help-view-source)
                               ("q" quit-window)
                               ("e" help-go-back)
                               ("r" help-go-forward)
                               ("g" revert-buffer))))

(use-package helpful
  :defer t
  :init
  (keymap-global-set "C-h v" #'helpful-variable)
  (keymap-global-set "C-h f" #'helpful-callable)
  (keymap-global-set "C-h k" #'helpful-key)

  (with-eval-after-load "ryo-modal"
    (ryo-modal-keys ("SPC"
                     (("j"
                       (("b" helpful-command)
                        ("k" helpful-callable)
                        ("l" helpful-variable)
                        ("v" helpful-key)))))))
  :config
  (with-eval-after-load "ryo-modal"
    (ryo-modal-major-mode-keys 'helpful-mode
                               ("q" quit-window)
                               ("g" helpful-update)
                               ("e" backward-button)
                               ("r" forward-button)
                               ("s" push-button))))

(use-package warnings
  :defer t
  :custom ((warning-minimum-level :error)))

(use-package subword
  :blackout
  :hook (on-first-input-hook . global-subword-mode))

(use-package paren
  :hook (on-first-input-hook . show-paren-mode)
  :config
  (setopt show-paren-when-point-inside-paren t))

(use-package elec-pair
  :hook (on-first-input-hook . electric-pair-mode))

(use-package electric
  :defer t
  :config
  (electric-indent-mode 0))

(use-package delsel
  :hook (on-first-input-hook . delete-selection-mode))

(use-package repeat
  :hook (on-first-input-hook . repeat-mode))

(use-package vc-git
  :defer t
  :config
  (with-eval-after-load "ryo-modal"
    (ryo-modal-major-mode-keys 'vc-git-log-view-mode
                               ("q" quit-window))))

(use-package vc-dir
  :defer t
  :config
  (with-eval-after-load "ryo-modal"
    (ryo-modal-major-mode-keys 'vc-dir-mode
                               ("q" quit-window)
                               ("g" revert-buffer)
                               ("i" vc-dir-previous-line)
                               ("k" vc-dir-next-line)
                               ("o" vc-dir-next-directory)
                               ("u" vc-dir-previous-directory)
                               ("s" vc-dir-find-file)
                               ("e" vc-dir-mark)
                               ("r" vc-dir-unmark)
                               ("v" vc-next-action)
                               ("p" vc-push)
                               ("=" vc-diff)
                               ("x" vc-dir-hide-up-to-date))))

(use-package vc-annotate
  :defer t
  :config
  (with-eval-after-load "ryo-modal"
    (ryo-modal-major-mode-keys 'vc-annotate-mode
                               ("q" quit-window)
                               ("g" revert-buffer))))

(use-package magit
  :defer t
  :init
  (with-eval-after-load "ryo-modal"
    (ryo-modal-key "SPC / m" #'magit-project-status))
  :bind ( :map project-prefix-map
          ("v" . magit-project-status)))

(use-package autoinsert
  :hook (on-first-file-hook . auto-insert-mode)
  :config
  (defun jacob-define-auto-insert (condition action)
    "Set up an auto insert idempotently.

CONDITION and ACTION are as in `define-auto-insert'."
    (when (assoc condition auto-insert-alist)
      (setopt auto-insert-alist (seq-remove (lambda (ai)
                                              (equal condition (car ai)))
                                            auto-insert-alist)))
    (define-auto-insert condition action))

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
  (when jacob-is-mac
    ;; problems on mac mean git grep doesn't work :(
    (setq-default dumb-jump-force-searcher 'grep)))

(use-package eglot
  :defer t
  :init
  (with-eval-after-load "ryo-modal"
    (ryo-modal-key "SPC SPC c e" #'eglot))
  :config
  (with-eval-after-load "ryo-modal"
    (ryo-modal-key "SPC SPC c r" #'eglot-rename))
  
  (jacob-defhookf eglot-managed-mode-hook
    (eglot-inlay-hints-mode 0)
    (setq-local eldoc-documentation-strategy 'eldoc-documentation-compose-eagerly)
    (setq-local xref-backend-functions '(eglot-xref-backend dumb-jump-xref-activate t)))

  (defun jacob-remove-ret-character-from-buffer (&rest _)
    "Remove all occurances of ^M from the buffer.

    Useful for deleting ^M after `eglot-code-actions'."
    (save-excursion
      (goto-char (point-min))
      (while (search-forward (char-to-string 13) nil t)
        (replace-match ""))))

  (defun jacob-eglot-xref-backend ()
    "Custom eglot xref backend.
Disables the eglot backend when inside a `.g8' template."
    (unless (string-match-p ".g8" default-directory)
      'eglot))

  (advice-add 'eglot-xref-backend :override #'jacob-eglot-xref-backend)

  (advice-add 'eglot-code-actions :after #'jacob-remove-ret-character-from-buffer)
  (advice-add 'eglot-rename :after #'jacob-remove-ret-character-from-buffer)

  ;; TODO: this causes problems for scala metals
  ;; (advice-add 'eglot-code-actions :after #'revert-buffer)

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

  (push '((gdscript-mode gdscript-ts-mode) "localhost" 6008) eglot-server-programs)

  (add-to-list 'eglot-server-programs '(scala-ts-mode . ("metals" "-Dmetals.http=on")))

  (eglot--code-action eglot-code-action-organize-imports-ts "source.organizeImports.ts")
  (eglot--code-action eglot-code-action-add-missing-imports-ts "source.addMissingImports.ts")

  (setopt eglot-ignored-server-capabilities '(:documentOnTypeFormattingProvider)
          eglot-stay-out-of '(imenu)
          eglot-code-action-indications '(eldoc-hint mode-line)))

(require 'jacob-csharp-mode)

(require 'jacob-sharper)

(use-package csproj-mode
  :mode ("\\.csproj\\'" . csproj-mode))

(use-package font-lock-ext ; dependency of `sln-mode'
  :defer t)

;; TODO: package `sln-mode' for elpa/melpa?
(use-package sln-mode
  :mode ("\\.sln\\'" . sln-mode))

(use-package fsharp-mode
  :defer t
  :mode ("\\.fs\\'" . fsharp-mode)
  :config
  (remove-hook 'project-find-functions #'fsharp-mode-project-root)
  (setopt compilation-error-regexp-alist (remq 'fsharp compilation-error-regexp-alist)))

(use-package scala-ts-mode
  :defer t
  :hook ((scala-ts-mode-hook . yas-minor-mode)
         (scala-ts-mode-hook . electric-indent-local-mode))
  :config
  (defun jacob-bloop-compile ()
    "Recompile the project with bloop."
    (interactive)
    (let ((default-directory (project-root (project-current))))
      (shell-command "bloop clean")
      (async-shell-command "bloop compile")))

  (defun jacob-scala-test-file ()
    "Test the current file."
    (interactive)
    (let ((package (treesit-node-text
                    (car
                     (treesit-query-capture (treesit-buffer-root-node)
                                            '((package_clause name: (package_identifier (identifier) @x)))
                                            nil
                                            nil
                                            "NODE_ONLY"))))
          (class (treesit-node-text
                  (car
                   (treesit-query-capture (treesit-buffer-root-node)
                                          '((class_definition name: (identifier) @x))
                                          nil
                                          nil
                                          "NODE_ONLY"))))
          (default-directory (project-root (project-current))))
      (compile (format "sbt \"testOnly %s.%s\"" package class))))

  (defun jacob-scala-dollar ()
    "Insert a dollar. If inside a string, enable string interpolation."
    (interactive)
    (unless (eq major-mode 'scala-ts-mode)
      (user-error "Not in a `scala-ts-mode' buffer"))
    (insert "$")
    (let* ((string-node (treesit-parent-until (treesit-node-at (point))
                                              "string"
                                              "INCLUDE-NODE"))
           (interpolated-string-node (treesit-parent-until string-node
                                                           "interpolated_string"
                                                           "INCLUDE-NODE")))
      (when (and string-node
                 (not interpolated-string-node))
        (save-excursion
          (goto-char (treesit-node-start string-node))
          (insert "s")))))

  (defun jacob-scala-determine-file-package ()
    "Return the package of the current scala file."
    (let* ((project-directory (locate-dominating-file (buffer-file-name)
                                                      (lambda (directory)
                                                        (seq-find (lambda (file)
                                                                    "Return non-nil if the file is a build.sbt file."
                                                                    (string-match-p "^build\\.sbt$" file))
                                                                  (directory-files directory)))))
           (file-relative-path (file-name-concat
                                (file-name-nondirectory (directory-file-name (file-name-directory project-directory)))
                                (file-relative-name (buffer-file-name) project-directory)))
           (file-relative-path-sans-src-dir (apply #'file-name-concat (cddr (file-name-split file-relative-path)))))
      (string-replace "/" "." (directory-file-name (file-name-directory file-relative-path-sans-src-dir)))))

  (keymap-set scala-ts-mode-map "$" #'jacob-scala-dollar)

  (add-hook 'scala-ts-mode-hook #'jacob-font-lock-scala-setup)

  (defun jacob-format-routes-file ()
    "Format a routes file."
    (interactive)
    (let ((regexp "^\\([^[:space:]]+\\)\\([[:space:]]+\\)\\([^[:space:]]+\\)\\([[:space:]]+\\)"))
      (align-regexp (point-min) (point-max) regexp 2 8)
      (align-regexp (point-min) (point-max) regexp 4 8))))

(use-package sbt-mode
  :defer t)

(use-package web-mode
  :mode ("\\.scala\\.html\\'" . web-mode)
  :custom (web-mode-engines-alist
           '(("play" . "\\.scala\\.html\\'"))))

(require 'jacob-dired)

(use-package nerd-icons-dired
  :blackout
  :hook (dired-mode-hook . nerd-icons-dired-mode))

(use-package nerd-icons-mode-line
  :hook (on-first-file-hook . nerd-icons-mode-line-global-mode))

(use-package esh-mode
  :defer t
  :config
  (defun jacob-async-eshell-command ()
    "Run an async command through eshell."
    (interactive)
    (let* ((command (read-from-minibuffer "Emacs shell command: "))
           (dir (if (project-current)
                    (project-root (project-current))
                  default-directory))
           (buffer-name (concat "*" dir ":" command "*")))
      (kill-buffer buffer-name)
      (eshell-command (concat command " &"))
      (with-current-buffer (get-buffer "*Eshell Async Command Output*")
        (rename-buffer buffer-name))))

  (when jacob-is-windows
    (defun jacob-confirm-terminate-batch-job ()
      "Type y and enter to terminate batch job after sending ^C."
      (when (not (null eshell-process-list))
        (insert "y")
        (eshell-send-input)))

    (advice-add 'eshell-interrupt-process :after #'jacob-confirm-terminate-batch-job)))

(use-package eldoc
  :hook (prog-mode-hook . global-eldoc-mode)
  :blackout
  :config
  (setopt eldoc-documentation-strategy 'eldoc-documentation-compose))

(use-package project
  :defer t
  :custom
  (project-switch-commands '((project-find-file "Find file")
                             (jacob-project-search "Find regexp")
                             (project-find-dir "Find directory")
                             (magit-project-status "Version Control" "v")
                             (project-eshell "Shell")
                             (project-compile "Compile"))))

(use-package prodigy
  :defer t
  :init
  (keymap-set project-prefix-map "l" #'prodigy)
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

  (add-hook 'prodigy-mode-hook #'hl-line-mode)

  (with-eval-after-load "ryo-modal"
    (ryo-modal-major-mode-keys 'prodigy-mode
                               ("d" prodigy-stop)
                               ("e" prodigy-mark)
                               ("g" jacob-project-search)
                               ("f" project-find-file)
                               ("i" prodigy-prev)
                               ("k" prodigy-next)
                               ("q" quit-window)
                               ("r" prodigy-unmark)
                               ("s" prodigy-restart)
                               ("v" prodigy-display-process)))

  (add-hook 'prodigy-view-mode-hook #'compilation-minor-mode)

  (with-eval-after-load "ryo-modal"
    (ryo-modal-major-mode-keys 'prodigy-view-mode
                               ("q" quit-window)
                               ("g" prodigy-restart))))

(use-package hi-lock
  :blackout)

(use-package highlight-defined
  :disabled
  :hook (emacs-lisp-mode-hook . highlight-defined-mode))

(use-package hl-todo
  :hook (after-init-hook . global-hl-todo-mode))

(use-package lisp-extra-font-lock
  :disabled
  :hook ((emacs-lisp-mode-hook lisp-mode-hook scheme-mode-hook) . lisp-extra-font-lock-mode))

(defun jacob-font-lock-scala-setup ()
  "Setup faces locally for scala."
  (dolist (face '(font-lock-keyword-face
                  font-lock-variable-use-face
                  font-lock-function-call-face
                  font-lock-preprocessor-face
                  font-lock-property-use-face
                  font-lock-builtin-face))
    (face-remap-add-relative face :foreground (face-foreground 'default)))

  (face-remap-add-relative 'font-lock-comment-face
                           :inherit 'font-lock-warning-face))

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

(use-package elisp-mode
  :defer t
  ;; :hook (emacs-lisp-mode-hook . flymake-mode)
  :hook ((emacs-lisp-mode-hook . jacob-font-lock-programming-setup)
         (emacs-lisp-mode-hook . yas-minor-mode)
         (emacs-lisp-mode-hook . electric-indent-local-mode))
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
  (keymap-set lisp-interaction-mode-map "(" #'insert-parentheses)
  (keymap-set lisp-interaction-mode-map ")" #'move-past-close-and-reindent)

  (keymap-set emacs-lisp-mode-map "(" #'insert-parentheses)
  (keymap-set emacs-lisp-mode-map ")" #'move-past-close-and-reindent)

  (setopt elisp-flymake-byte-compile-load-path load-path)

  (font-lock-add-keywords 'emacs-lisp-mode
                          '(("(use-package \\([[:word:]-]+\\)" 1 'font-lock-function-name-face))))

(use-package scheme
  :mode ("\\.scm\\'" . scheme-mode)
  :config
  (keymap-set scheme-mode-map "(" #'insert-parentheses)
  (keymap-set scheme-mode-map ")" #'move-past-close-and-reindent))

(use-package geiser
  :after scheme)

(use-package geiser-mode
  :after (scheme geiser)
  :config
  (with-eval-after-load "ryo-modal"
    (ryo-modal-major-mode-keys 'geiser-mode
                               ("SPC , m" geiser-eval-last-sexp)
                               ("SPC , d" geiser-eval-definition))))

(use-package geiser-guile
  :after (scheme geiser))

(use-package mermaid-mode
  :mode ("\\.mermaid\\'"))

(use-package ob-mermaid
  :after org)

(use-package org
  :mode ("\\.org\\'" . org-mode)
  :config
  (defun jacob-org-babel-tangle-delete-whitespace ()
    "Get rid of the whitespace at the end of the buffer."
    (goto-char (point-max))
    (delete-trailing-whitespace)
    (backward-delete-char 1)
    (save-buffer))

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
    (toggle-word-wrap 1)))

(use-package org-agenda
  :commands (org-agenda org-capture)
  :config
  (setopt org-agenda-skip-scheduled-if-done t
          org-agenda-skip-deadline-if-done t
          org-agenda-custom-commands '(("r" "Routine" agenda "" ((org-agenda-tag-filter-preset '("+tickler"))
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

  (with-eval-after-load "ryo-modal"
    (ryo-modal-major-mode-keys 'org-agenda-mode
                               ("q" quit-window)
                               ("g" org-agenda-redo-all)))

  (advice-add #'org-agenda :before #'jacob-generate-daily-tasks))

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
  :disabled
  :defer t
  :init
  (defun jacob-pulse-line (&rest _)
    "Pulse the current line."
    (pulse-momentary-highlight-region (save-excursion
                                        (back-to-indentation)
                                        (point))
                                      (line-end-position)))

  (dolist (command '(recenter-top-bottom
                     scroll-up-command
                     scroll-down-command
                     other-window
                     jacob-split-or-switch-window
                     xref-find-definitions
                     xref-pop-marker-stack
                     isearch-done))
    (advice-add command :after #'jacob-pulse-line))

  (defun jacob-pulse-defun (&rest _)
    "Pulse the defun at point."
    (let ((bounds (bounds-of-thing-at-point 'defun)))
      (pulse-momentary-highlight-region (car bounds) (cdr bounds))))

  (advice-add #'eval-defun :after #'jacob-pulse-defun))

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
  (with-eval-after-load "ryo-modal"
    (ryo-modal-major-mode-keys 'calendar-mode
                               ("q" quit-window)
                               ("i" calendar-backward-week)
                               ("k" calendar-forward-week)
                               ("j" calendar-backward-day)
                               ("l" calendar-forward-day)
                               ("u" calendar-backward-month)
                               ("o" calendar-forward-month)
                               ("d" diary-view-entries)
                               ("s" diary-insert-entry)
                               ("m" diary-mark-entries)
                               ("." calendar-goto-today)
                               ("t" calendar-set-mark)))

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
  (with-eval-after-load "ryo-modal"
    (ryo-modal-major-mode-keys 'compilation-mode
                               ("g" recompile)
                               ("q" quit-window)))

  (add-hook 'compilation-filter-hook #'ansi-color-compilation-filter)

  (defun jacob-compilation-project-file ()
    "Somehow determine the filepath for the compilation error"
    (save-match-data
      (let (
            (filename-from-error (match-string 1)) ;; FIXME: this will break when filename is not in first re group
            )
        (seq-find (lambda (f)
                    (string= (file-name-nondirectory f)
                             filename-from-error))
                  (project-files (project-current))))))

  (add-to-list 'compilation-error-regexp-alist-alist
               '(jacob-sbt-test-fail-re
                 ".* (\\([a-zA-Z\\.]+\\):\\([0-9]+\\))"
                 jacob-compilation-project-file
                 2))

  (add-to-list 'compilation-error-regexp-alist 'jacob-sbt-test-fail-re)

  ;; TODO: adapt regexp to include compilation error within sbt test
  (add-to-list 'compilation-error-regexp-alist-alist
               '(jacob-sbt-error
                 "^\\[error][[:space:]]--[[:space:]].*Error: \\([^:]+\\):\\([[:digit:]]+\\):\\([[:digit:]]+\\)" 1 2 3 nil 1))

  (add-to-list 'compilation-error-regexp-alist 'jacob-sbt-error)

  (add-to-list 'compilation-error-regexp-alist-alist
               '(jacob-sbt-warning
                 "^\\[warn][[:space:]]--[[:space:]].*Warning: \\([^:]+\\):\\([[:digit:]]+\\):\\([[:digit:]]+\\)" 1 2 3 1 1))

  (add-to-list 'compilation-error-regexp-alist 'jacob-sbt-warning)

  (setopt compilation-always-kill t
          compilation-scroll-output t
          compilation-ask-about-save nil))

(use-package winnow
  :hook (compilation-mode-hook . winnow-mode))

(require 'jacob-sql)

(use-package doc-view
  :defer t
  :config
  (with-eval-after-load "ryo-modal"
    (ryo-modal-major-mode-keys 'doc-view-mode
                               ("l" doc-view-next-page)
                               ("j" doc-view-previous-page))))

(use-package treesit
  :defer t
  :custom
  (treesit-font-lock-level 4))

(use-package treesit-auto
  :hook (prog-mode-hook . global-treesit-auto-mode)
  :config
  (add-to-list 'treesit-auto-recipe-list (make-treesit-auto-recipe
                                          :lang 'gdscript
                                          :ts-mode 'gdscript-ts-mode
                                          :remap 'gdscript-mode
                                          :url "https://github.com/PrestonKnopp/tree-sitter-gdscript.git"
                                          :ext "\\.gd\\'"))

  (add-to-list 'treesit-auto-langs 'gdscript)
  
  (treesit-auto-add-to-auto-mode-alist 'all))

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
  :defer t
  :init
  (add-to-list 'auto-mode-alist '("Directory.Packages.props" . nxml-mode)))

(use-package avy
  :defer t
  :init
  (keymap-global-set "M-j" #'avy-goto-char-timer)
  (keymap-set isearch-mode-map "M-j" #'avy-isearch)
  :config
  (defun jacob-avy-xref (point)
    "Call `xref-find-definitions' at POINT."
    (goto-char point)
    (call-interactively #'xref-find-definitions)
    (jacob-avy-go-home))

  (defun jacob-avy-kill-line (point)
    "Kill line at POINT."
    (save-excursion
      (goto-char point)
      (beginning-of-line)
      (kill-line)
      (delete-char 1))
    (jacob-avy-go-home))

  (defun jacob-avy-copy-line (point)
    "Copy line at POINT."
    (save-excursion
      (goto-char point)
      (copy-region-as-kill (line-beginning-position) (line-end-position)))
    (jacob-avy-go-home))

  (defun jacob-avy-yank-line (pt)
    "Copy sexp starting on PT."
    (jacob-avy-copy-line pt)
    (yank))

  (defun jacob-avy-go-home ()
    "Return to the avy origin."
    (let ((dat (ring-ref avy-ring 0)))
      (select-frame-set-input-focus
       (window-frame (cdr dat)))
      (select-window (cdr dat))
      (goto-char (car dat))))

  (defun jacob-avy-eglot-rename (pt)
    "Copy sexp starting on PT."
    (save-excursion
      (goto-char pt)
      (call-interactively #'eglot-rename))
    (jacob-avy-go-home))

  (setopt avy-keys '(?a ?s ?d ?f ?g ?h ?j ?l ?\;)
          avy-dispatch-alist '((?v . avy-action-yank)
                               (?V . jacob-avy-yank-line)
                               (?x . avy-action-kill-stay)
                               (?X . jacob-avy-kill-line)
                               (?t . avy-action-mark)
                               (?c . avy-action-copy)
                               (?C . jacob-avy-copy-line)
                               (?i . avy-action-ispell)
                               (?z . avy-action-zap-to-char)
                               (?. . jacob-avy-xref)
                               (?r . jacob-avy-eglot-rename))))

(require 'jacob-apheleia)

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

(use-package visual-replace
  :defer t
  :init
  (with-eval-after-load "ryo-modal"
    (ryo-modal-keys
     ("SPC"
      (("r" visual-replace)
       ("k"
        (("r" visual-replace-regexp)))))))
  :hook ((visual-replace-minibuffer-mode-hook . visual-replace-toggle-query)))

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

(use-package marginalia
  :hook (jacob-first-minibuffer-activation-hook . marginalia-mode))

;; FIXME: ensure all elisp files have lexical binding enabled, figure
;; out how to automate adding the lexical binding cookie upon creation
;; of new files.

(require 'jacob-consult)

(use-package consult-imenu
  :bind ("M-g i" . consult-imenu))

(use-package embark
  :defer t
  :init
  (with-eval-after-load "ryo-modal"
    (ryo-modal-key "\\" #'embark-act)
    (ryo-modal-major-mode-keys 'embark-collect
                               ("q" quit-window)))
  :config
  (setopt embark-cycle-key "\\"
          embark-help-key "h")

  (keymap-set embark-flymake-map "a" #'eglot-code-actions)
  (push 'embark--ignore-target (alist-get 'eglot-code-actions embark-target-injection-hooks))

  (keymap-set embark-identifier-map "r" #'eglot-rename)
  (push 'embark--ignore-target (alist-get 'eglot-rename embark-target-injection-hooks))

  (keymap-unset embark-general-map "w")
  (keymap-set embark-general-map "c" #'embark-copy-as-kill)

  (defvar-keymap embark-variable-map
    :doc "Keymap for Embark variable actions."
    :parent embark-symbol-map
    "=" #'set-variable
    "s" #'customize-set-variable        ; modified
    "u" #'customize-variable
    "v" #'embark-save-variable-value
    "<" #'embark-insert-variable-value
    "t" #'embark-toggle-variable)

  (keymap-set embark-general-map "x" #'kill-region))

(use-package embark-consult
  :defer t
  :after (:and embark consult))

(use-package cape
  :init
  (add-to-list 'completion-at-point-functions #'cape-dabbrev))

(use-package expreg
  :defer t
  :init
  (with-eval-after-load "ryo-modal"
    (ryo-modal-key "g" #'expreg-expand))
  :config
  (setopt expreg-functions (delq 'expreg--subword expreg-functions)))

(use-package verb
  :after org
  :hook (org-mode-hook . verb-mode)
  :config
  (with-eval-after-load "ryo-modal"
    (ryo-modal-major-mode-keys 'verb-response-body-mode
                               ("q" quit-window)))

  (defun jacob-verb-id (response-id)
    "Get the id property from the stored verb response pertaining to RESPONSE-ID."
    (verb-json-get (oref (verb-stored-response response-id) body) "id")))

(use-package sly
  :hook (lisp-mode-hook . sly-mode)
  :config
  (sly-setup)

  (sly-symbol-completion-mode 0)

  (with-eval-after-load "ryo-modal"
    (ryo-modal-major-mode-keys 'sly-mode
                               ("SPC , m" sly-eval-last-expression)
                               ("SPC , d" sly-eval-defun)
                               ("SPC , e" sly-eval-buffer)
                               ("SPC w k" sly-edit-definition)))

  (setopt sly-auto-start 'always
          inferior-lisp-program "sbcl")

  (ryo-modal-major-mode-keys 'sly-db
                             ("q" sly-db-quit)))

(use-package sly-overlay
  :after sly)

(use-package sly-macrostep
  :after sly)

;; (jacob-require sly-stepper "https://github.com/joaotavora/sly-stepper.git")

(use-package sly-quicklisp
  :after sly)

(use-package sql-indent
  :hook (sql-mode-hook . sqlind-minor-mode))

(require 'jacob-gptel)

(use-package aider
  :defer t
  ;; (setenv "GEMINI_API_KEY" "")
  :custom ((aider-args '("--model" "gemini/gemini-2.0-flash-exp" "--edit-format" "whole"))))

(use-package gdscript-mode
  :mode ("\\.gd\\'" . gdscript-ts-mode)
  :config
  (add-hook 'gdscript-ts-mode-hook #'indent-tabs-mode))

(use-package eat
  :when jacob-is-linux
  :defer t
  :init
  (add-hook 'eshell-mode-hook #'eat-eshell-mode))

(use-package exec-path-from-shell
  :if jacob-is-mac
  :config
  (add-to-list 'exec-path-from-shell-variables "JAVA_HOME")
  (exec-path-from-shell-initialize))

(use-package pdf-tools
  :when jacob-is-linux
  :mode ("\\.[pP][dD][fF]\\'" . pdf-view-mode)
  :config
  (with-eval-after-load "ryo-modal"
    (ryo-modal-major-mode-keys 'pdf-view-mode
                               ("c" pdf-view-kill-ring-save))))

(use-package grep
  :defer t
  :config
  (when jacob-is-windows
    (setq find-program "C:/Program Files (x86)/GnuWin32/bin/find.exe")))


;; personal functions

(require 'jacob-rofi)

(define-minor-mode jacob-screen-sharing-mode
  "Minor mode for sharing screens."
  :global t
  :group 'jacob
  (let ((on (if jacob-screen-sharing-mode 1 0)))
    (global-hl-line-mode on)
    (global-display-line-numbers-mode on)))

(defun jacob-ip-to-kill-ring ()
  "Copy v4 ip address to kill ring."
  (interactive)
  (kill-new (with-temp-buffer (shell-command "curl --silent -4 ifconfig.me"
                                             t)
                              (buffer-string))))

(defun jacob-random-init ()
  "Go to a random place in init file."
  (interactive)
  (find-file user-init-file)
  (goto-char (random (point-max))))

(defun jacob-async-shell-command (command)
  "Wrapper command for (`async-shell-command' COMMAND)."
  (interactive
   (list
    (read-shell-command (if shell-command-prompt-show-cwd
                            (format-message "Async shell command in `%s': "
                                            (abbreviate-file-name
                                             default-directory))
                          "Async shell command: ")
                        nil nil
                        (let ((filename
                               (cond
                                (buffer-file-name)
                                ((eq major-mode 'dired-mode)
                                 (dired-get-filename nil t)))))
                          (and filename (file-relative-name filename))))))
  (async-shell-command command
                       (format "* %s %s *" default-directory command)))

(defvar jacob-format-words-style-and-start nil
  "Pair of currently selected style and starting point.
If nil, means you havent used the command for the first time yet.")

(defun jacob-format-words ()
  "Command for formating words into identifiers when writing code.

On first use, ask for formatting style (e.g. kebab, camel,
etc).  Format one word backwards in selected style and store the style
and the position of point after formatting, return point to where it
was when command called.

On consecutive use, apply stored formatting to word before stored
point."
  (interactive)

  (undo-boundary)

  (unless (eq last-command this-command)
    (setq jacob-format-words-style-and-start (cons (read-char-from-minibuffer "select style: " '(?c ?p ?k ?s ?S))
                                                   (point))))

  (save-excursion
    (let* ((style (car jacob-format-words-style-and-start))
           (format-position (cdr jacob-format-words-style-and-start)))
      (goto-char format-position)
      (pcase style
        (?c (progn
              (backward-word)
              (capitalize-word 1)
              (backward-word)
              (backward-delete-char 1)))
        (?p (progn
              (backward-word)
              (capitalize-word 1)
              (backward-word)
              (backward-delete-char 1)
              (backward-word)
              (capitalize-word 1)))
        (?k (progn
              (backward-word)
              (backward-delete-char 1)
              (insert-char ?-)
              (backward-char)))
        (?s (progn
              (backward-word)
              (backward-delete-char 1)
              (insert-char ?_)
              (backward-char)))
        (?S (progn
              (backward-word)
              (upcase-word 1)
              (backward-word)
              (backward-delete-char 1)
              (insert-char ?_)
              (backward-char)
              (backward-word)
              (upcase-word 1))))

      (setq jacob-format-words-style-and-start (cons style (point))))))

(defun jacob-eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))

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

(defvar jacob-gitlab-push-set-upstream-jira-url ""
  "URL for current employer's jira board.")

(defun jacob-gitlab-push-set-upstream ()
  "Push the current branch and create an upstream branch.
Use GitLab push options to create a merge request and set all
necessary values.

For use with GitLab only."
  (interactive)
  (let* ((branch-name (with-temp-buffer
                        (eshell-command "git symbolic-ref HEAD --short" t)
                        (buffer-substring-no-properties (point-min) (- (point-max) 1))))
         (mr-key (progn
                   (string-match (rx (+ alpha) "-" (+ digit))
                                 branch-name)
                   (match-string 0 branch-name)))
         (jira-link (concat jacob-gitlab-push-set-upstream-jira-url mr-key))
         (command (concat "git push --set-upstream origin HEAD "
                          (concat "-o merge_request.create "
                                  "-o merge_request.remove_source_branch "
                                  (concat "-o merge_request.title=\""
                                          branch-name
                                          "\" ")
                                  (concat "-o merge_request.description=\""
                                          "[" mr-key "](" jira-link ")"
                                          "\" ")))))
    (with-temp-buffer
      (eshell-command command t))))

(defun jacob-generate-daily-tasks (&rest _args)
  "Prepare your daily tasks."
  ;; TODO: make file regenerate after 6 AM
  ;; (let* ((now (decode-time))
  ;;      (tomorrow-day (1+ (decoded-time-day now)))
  ;;      (month (decoded-time-month now))
  ;;      (year (decoded-time-year now)))
  ;; (encode-time (list 0 0 6 tomorrow-day month year)))
  (let* ((tasks
          '("Clean Teeth AM"
            "Clean Teeth PM"
            "Take Mesalazine"
            "Take Vitamin D"
            "Use Inhaler AM"
            "Use Inhaler PM"))
         (file-path (file-name-concat org-directory "daily-tasks.org"))
         (today (format-time-string "%Y-%m-%d %a"))
         (last-generated-date (with-temp-buffer
                                (insert-file-contents file-path)
                                (goto-char (point-min))
                                (re-search-forward "#\\+DATE: <\\([a-zA-Z0-9 -]+\\)>")
                                (match-string 1)))
         (will-generate-file (> (days-between today last-generated-date)
                                0)))
    (when will-generate-file
      (with-temp-file file-path
        (delete-region (point-min) (point-max))
        (insert "#+TITLE: Daily Tasks\n")
        (insert "#+AUTHOR: Emacs Lisp Script\n")
        (insert "#+DATE: <" today ">\n\n")
        (dolist (task tasks)
          (insert (format "* TODO %s :daily:\n" task))
          (insert "SCHEDULED: <" today ">\n\n"))))))

(defun jacob-update-config ()
  "Update your Emacs configuration with git."
  (interactive)
  (let ((default-directory "~/.emacs.d/"))
    (shell-command "git stash")
    (shell-command "git pull")
    ;; (shell-command "git push")
    (shell-command "git stash pop")))

(provide 'init)

;;; init.el ends here
