;;; init.el --- Jacob's main init file. -*-lexical-binding: t-*-
;;; Commentary:
;;; Code:

(add-to-list 'load-path (concat (file-name-directory user-init-file)
                                "lisp"))

;; use-package
(require 'use-package)

(setopt use-package-enable-imenu-support t
        use-package-verbose t
        use-package-compute-statistics t
        use-package-hook-name-suffix nil)

(use-package package
  :custom (package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                              ("nongnu" . "https://elpa.nongnu.org/nongnu/")
                              ("melpa" . "https://melpa.org/packages/"))))

(use-package no-littering
  :ensure t)

(use-package on
  :ensure t)

(keymap-global-unset "<menu-bar> <options>")

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
                           keymap)))

(tool-bar-add-item "exit"
                   #'keyboard-quit
                   'keyboard-quit)

(tool-bar-add-item "next-node"
                   #'other-window
                   'other-window)

(keymap-global-unset "<tool-bar> <open-file>")
(keymap-global-unset "<tool-bar> <dired>")
(keymap-global-unset "<tool-bar> <save-buffer>")

;;(x-popup-menu t '("jacobtest" ("jacobtest2" ("key" . "value"))))

;; read environment file and variable setup

(defvar jacob-font-size 11
  "Font size to use.")

(defconst jacob-is-windows (eq system-type 'windows-nt)
  "Is the current OS windows?")

(defconst jacob-is-linux (eq system-type 'gnu/linux)
  "Is the current OS linux?")

(when (file-exists-p "~/.emacs.d/environment.el")
  (load-file "~/.emacs.d/environment.el"))

;; custom hooks

(defvar jacob-first-minibuffer-activation-hook '()
  "Hook for first time minibuffer activated.")

(defun jacob-run-first-minibuffer-activation-hook (&rest _args)
  "Run `jacob-first-minibuffer-activation-hook';
then remove this function from `find-file-hook'."
  (when (member 'init features)
    (run-hooks 'jacob-first-minibuffer-activation-hook)
    (advice-remove #'jacob-run-first-minibuffer-activation-hook #'completing-read)))

(advice-add #'completing-read :before #'jacob-run-first-minibuffer-activation-hook)

(use-package emacs
  :config
  ;; c code
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
  (tab-width 4) ; set default tab char's display width to 4 spaces
  (truncate-lines (cond (jacob-is-android t)
                        (t nil)))
  (delete-by-moving-to-trash t)
  (read-process-output-max (* 1024 1024))
  (frame-resize-pixelwise t)
  (create-lockfiles nil)
  (history-length 1000)
  (history-delete-duplicates t)
  (scroll-conservatively 101)
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

(defmacro jacob-defhookf (hook &rest body)
  "Define function with BODY and bind it to HOOK."
  (declare (indent defun))
  (let* ((hook-name (symbol-name hook))
         (function-name (intern (concat "jacob-" hook-name "-function"))))
    `(progn
       (defun ,function-name ()
         ,(format "Auto-generated hook function for `%s'." hook-name)
         ,@body)
       (add-hook ',hook #',function-name))))

(use-package delight
  :ensure t)

(delight 'yas-minor-mode nil "yasnippet")
(delight 'xah-fly-keys " 🛪")

(use-package indent-aux
  :hook (after-init-hook . kill-ring-deindent-mode))

(use-package which-key
  :delight
  :hook (on-first-input-hook . which-key-mode)
  :custom (which-key-idle-delay (cond (jacob-is-android 1)
                                      (t 0.01))))

(use-package mwheel
  :custom ((mouse-wheel-progressive-speed nil)
           (mouse-wheel-scroll-amount '(10 ((shift) . hscroll)
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
  :delight
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

(use-package custom
  :config
  (load-theme 'modus-vivendi-tinted))

(use-package cus-edit
  :custom
  (custom-file (concat user-emacs-directory "custom.el"))
  :config
  (when (file-exists-p custom-file)
    (load custom-file)))

(use-package generic-x)          ; support for files like `/etc/fstab'

(use-package simple
  :config
  (column-number-mode 1)
  (line-number-mode 1)
  :custom
  (indent-tabs-mode nil)               ; use spaces to indent
  (save-interprogram-paste-before-kill t)
  :bind ("C-x u" . nil)                 ; `undo'
  )

(use-package bookmark
  :defer t
  :custom
  (bookmark-set-fringe-mark nil)
  (bookmark-watch-bookmark-file 'silent)
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
    (when (= 1 (point))
      (user-error "Beginning of buffer"))
    (let ((char-class (char-syntax (char-before)))
          (f (if current-prefix-arg
                 #'delete-pair
               #'kill-sexp)))
      (unless (ignore-errors
                (funcall jacob-backspace-function f))
        (cond ((= ?\" char-class)     ; string
               (if (nth 3 (syntax-ppss))
                   (backward-char)
                 (backward-sexp))
               (funcall f))
              ((= ?\( char-class)     ; delete from start of pair
               (backward-char)
               (funcall f))
              ((= ?\) char-class)     ; delete from end of pair
               (backward-sexp)
               (funcall f))
              (t                      ; delete character
               (backward-delete-char 1)))))))

(defun jacob-kill-line ()
  "If region is active, kill it.  Otherwise:

  If point is at the beginning of the line, kill the whole line.

  If point is at the end of the line, kill until the beginning of the line.

  Otherwise, kill from point to the end of the line."
  (interactive)
  (cond ((region-active-p)
         (call-interactively #'kill-region))
        ((bolp)
         (kill-whole-line))
        ((eolp)
         (kill-line 0))
        (t
         (kill-line))))

(keymap-global-unset "C-w")             ; `kill-region'
(keymap-global-unset "C-M-w")           ; `append-next-kill'

(defun jacob-kill-paragraph ()
  "Move to the beginning of the paragraph, then kill it."
  (interactive)
  (forward-paragraph)
  (backward-paragraph)
  (kill-paragraph 1))

(defun jacob-comment-dwim ()
  "Toggle comment on current line or active region."
  (interactive)
  (cond ((or (region-active-p)
             (= (point) (line-end-position)))
         (comment-dwim nil))
        (t
         (comment-or-uncomment-region (line-beginning-position) (line-end-position))
         (forward-line))))

(defun jacob-find-file (force-find-file)
  "If in project, call `project-find-file'. Otherwise, call `find-file'.

If FORCE-FIND-FILE is non-nil call `find-file'."
  (interactive "P")
  (call-interactively (cond (force-find-file #'find-file)
                            ((project-current) #'project-find-file)
                            (t #'find-file))))

(with-eval-after-load "xah-fly-keys"
  (keymap-set xah-fly-leader-key-map "i e" #'jacob-find-file))

(require 'jacob-xah-fly-keys)

(require 'jacob-yasnippet)

(use-package minibuffer
  :config
  (define-key minibuffer-local-completion-map "SPC" 'self-insert-command)

  (jacob-xfk-bind-for-mode minibuffer-setup
                           "g" #'embark-export))

(use-package ibuffer
  :defer t
  :config
  (with-eval-after-load "xah-fly-keys"
    (jacob-xfk-bind-for-mode ibuffer-mode
	                         "q" #'quit-window
                             "g" #'ibuffer-update)))

(use-package replace
  :config
  (jacob-xfk-bind-for-mode occur-mode
                           "q" 'quit-window
                           "i" 'occur-prev
                           "k" 'occur-next))

(use-package info
  :defer
  :config
  (jacob-xfk-bind-for-mode Info-mode
                           "q" #'quit-window
                           "r" #'Info-scroll-up
                           "e" #'Info-scroll-down
                           "w" #'Info-up
                           "g" #'Info-menu))

(use-package man
  :defer
  :custom
  (Man-notify-method 'pushy))

(use-package diff
  :config
  (jacob-xfk-bind-for-mode diff-mode
                           "q" #'quit-window
                           "e" #'diff-hunk-prev
                           "r" #'diff-hunk-next
                           "x" #'diff-hunk-kill
                           "g" #'revert-buffer))

(use-package help
  :defer t
  :config
  (setopt help-window-select t
          help-enable-variable-value-editing t))

(use-package help-fns
  :defer t
  :config
  (defun jacob-help-edit ()
    "Edit variable in current help buffer."
    (interactive)
    (unless (equal major-mode 'help-mode)
      (message "not in help buffer"))
    (save-excursion
      (goto-char (point-min))
      (if (search-forward "Its value is " nil "NOERROR")
          (help-fns-edit-variable)
        (message "cannot find editable variable")))))

(use-package help-mode
  :config
  (jacob-xfk-bind-for-mode help-mode
                           "s" #'help-view-source
                           "q" #'quit-window
                           "e" #'help-go-back
                           "r" #'help-go-forward
                           "g" #'revert-buffer
                           "w" #'jacob-help-edit))

(use-package helpful
  :ensure t
  :defer t
  :init
  (keymap-global-set "C-h v" #'helpful-variable)
  (keymap-global-set "C-h f" #'helpful-callable)
  (keymap-global-set "C-h k" #'helpful-key)

  (with-eval-after-load "xah-fly-keys"
    (keymap-set xah-fly-leader-key-map "j k" #'helpful-callable)
    (keymap-set xah-fly-leader-key-map "j l" #'helpful-variable)
    (keymap-set xah-fly-leader-key-map "j v" #'helpful-key)
    (keymap-set xah-fly-leader-key-map "j b" #'helpful-command))
  :config
  (jacob-xfk-bind-for-mode helpful-mode
                           "q" #'quit-window
                           "g" #'helpful-update
                           "e" #'backward-button
                           "r" #'forward-button
                           "s" #'push-button))

(use-package warnings
  :custom ((warning-minimum-level :error)))

(use-package subword
  :delight subword-mode
  :hook (on-first-input-hook . global-subword-mode))

(use-package paren
  :hook (on-first-input-hook . show-paren-mode)
  :config
  (setopt show-paren-when-point-inside-paren t))

(use-package elec-pair
  :hook (on-first-input-hook . electric-pair-mode))

(use-package delsel
  :hook (on-first-input-hook . delete-selection-mode))

(use-package repeat
  :hook (on-first-input-hook . repeat-mode))

(use-package dabbrev
  :defer t
  :config
  (setopt dabbrev-case-fold-search nil
          dabbrev-case-replace nil))

(use-package vc
  :defer t
  :config
  (setopt vc-git-show-stash 0             ; show 0 stashes
          vc-ignore-dir-regexp (format "\\(%s\\)\\|\\(%s\\)" ; ignore tramp files
                                       vc-ignore-dir-regexp
                                       tramp-file-name-regexp)))

(use-package vc-git
  :defer t
  :config
  (jacob-xfk-bind-for-mode vc-git-log-view-mode
                           "q" #'quit-window))

(use-package vc-dir
  :defer t
  :config
  (jacob-xfk-bind-for-mode vc-dir-mode
                           "q" #'quit-window
                           "g" #'revert-buffer
                           "i" #'vc-dir-previous-line
                           "k" #'vc-dir-next-line
                           "o" #'vc-dir-next-directory
                           "u" #'vc-dir-previous-directory
                           "s" #'vc-dir-find-file
                           "e" #'vc-dir-mark
                           "r" #'vc-dir-unmark
                           "v" #'vc-next-action
                           "p" #'vc-push
                           ";" #'jacob-git-push-set-upstream
                           "=" #'vc-diff
                           "x" #'vc-dir-hide-up-to-date))

(use-package vc-annotate
  :defer t
  :config
  (jacob-xfk-bind-for-mode vc-annotate-mode
                           "q" #'quit-window
                           "g" #'revert-buffer))

(use-package magit
  :ensure t
  :bind ( :map project-prefix-map
          ("v" . magit-project-status)))

(use-package forge
  :ensure t
  :defer t)

;; FIXME: trying to optimise this package causes git gutter stuff to
;; appear on the left margin
(use-package git-gutter-fringe
  :ensure t
  :delight git-gutter-mode              ; FIXME: not hidden in the mode line
  :config
  (global-git-gutter-mode 1)
  (setq git-gutter-fr:side 'right-fringe))

(use-package autoinsert
  :hook (on-first-file-hook . auto-insert-mode)
  :config
  (setopt auto-insert-query t))

(use-package tramp
  :defer t
  :config
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path)
  (setopt tramp-archive-enabled nil) ; lots of problems. for now, disable it!
  )

(use-package eglot
  :defer t
  :init
  (defvar-keymap jacob-code-map
    "e" #'eglot
    "a" #'eglot-code-actions
    "r" #'eglot-rename
    "i" #'eglot-find-implementation
    "t" #'eglot-find-typeDefinition)

  (with-eval-after-load "xah-fly-keys"
    (keymap-set jacob-xfk-map "c" `("Code" . ,jacob-code-map)))

  :config
  (jacob-defhookf eglot-managed-mode-hook
    (eglot-inlay-hints-mode 0)
    (setq-local eldoc-documentation-strategy 'eldoc-documentation-compose))

  (defun jacob-remove-ret-character-from-buffer (&rest _)
    "Remove all occurances of ^M from the buffer.

Useful for deleting ^M after `eglot-code-actions'."
    (save-excursion
      (goto-char (point-min))
      (while (search-forward (char-to-string 13) nil t)
        (replace-match ""))))

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

  (push '((gdscript-mode gdscript-ts-mode) "localhost" 6008) eglot-server-programs)

  (eglot--code-action eglot-code-action-organize-imports-ts "source.organizeImports.ts")
  (eglot--code-action eglot-code-action-add-missing-imports-ts "source.addMissingImports.ts")

  (setopt eglot-ignored-server-capabilities '(:documentOnTypeFormattingProvider)))

(use-package csharp-mode
  :mode ("//.csx?//'" . csharp-ts-mode)
  :config

  (defun jacob-csharp-determine-file-namespace ()
    "Return the namespace of the current file."
    (let* ((project-directory (locate-dominating-file (buffer-file-name)
                                                      (lambda (directory)
                                                        (seq-find (lambda (file)
                                                                    (string-match-p "\\.csproj$" file))
                                                                  (directory-files directory)))))
           (relative-path (file-name-concat
                           (file-name-nondirectory (directory-file-name (file-name-directory project-directory)))
                           (file-relative-name (buffer-file-name) project-directory))))
      (string-replace "/" "." (directory-file-name (file-name-directory relative-path)))))

  (defun jacob-csharp-fix-namespace ()
    "Fix the namespace of the current file."
    (interactive)
    (let* ((guessed-namespace (jacob-csharp-determine-file-namespace))
           (current-namespace-range (car (treesit-query-range
                                          (treesit-buffer-root-node)
                                          '((file_scoped_namespace_declaration name: (_) @x))))))
      (delete-region (car current-namespace-range)
                     (cdr current-namespace-range))
      (save-excursion
        (goto-char (car current-namespace-range))
        (insert guessed-namespace))))

  (defun jacob-csharp-move-file ()
    "Move the current file.

Update the class/record/interface name and the namespace to reflect the
new location and/or name of the file."
    (interactive)
    (let* ((new-file-name (read-file-name "Move file to: "
                                          nil
                                          buffer-file-name))
           (new-class-name (file-name-sans-extension
                            (file-name-nondirectory
                             new-file-name)))
           (class-name-range (car
                              (treesit-query-range (treesit-buffer-root-node)
                                                   '((class_declaration (identifier) @identifier))))))
      ;; 1. rename the file
      (rename-visited-file new-file-name)
      ;; 2. rename the class
      (save-excursion
        (delete-region (car class-name-range) (cdr class-name-range))
        (goto-char (car class-name-range))
        (insert new-class-name)))
    ;; 3. update the namespace
    (jacob-csharp-fix-namespace))

  (defun jacob-csharp-create-variable ()
    "Create a variable declaration statement for an undeclared variable."
    (interactive)
    (let* ((identifier
            (thing-at-point 'symbol "NO-PROPERTIES"))
           (first-occurance
            (seq-first (seq-sort #'<
                                 (mapcar #'treesit-node-start
                                         (mapcar #'cdr
                                                 (treesit-query-capture (csharp-toolbox--get-method-node)
                                                                        `(((identifier) @id (:equal @id ,identifier))))))))))
      (goto-char first-occurance)
      (goto-char (treesit-beginning-of-thing "_statement$"))
      (forward-line -1)
      (end-of-line)
      (newline 1 "INTERACTIVE")
      (insert (format "var %s = Guid.NewGuid();" identifier))))

  (defun jacob-csharp-forward-statement ()
    "Move forward over a csharp statement."
    (interactive)
    (treesit-end-of-thing "statement"))

  (defun jacob-csharp-backward-statement ()
    "Move backward over a csharp statement."
    (interactive)
    (treesit-beginning-of-thing "statement"))

  (defun jacob-csharp-beginning-of-line-or-statement ()
    "Move cursor to the beginning of line or previous csharp statement."
    (interactive)
    (let ((p (point)))
      (if (eq last-command this-command)
          (call-interactively 'jacob-csharp-backward-statement)
        (back-to-indentation)
        (when (eq p (point))
          (beginning-of-line)))))

  (defun jacob-csharp-end-of-line-or-statement ()
    "Move cursor to the end of line or next csharp statement."
    (interactive)
    (if (eq last-command this-command)
        (call-interactively 'jacob-csharp-forward-statement)
      (end-of-line)))

  (defun jacob-backspace-csharp (f)
    "Function for `jacob-backspace' to help with csharp.

Figure out if the `<' or `>' before point is part of a
`type_argument_list', and delete accordingly.  F is a function
which performs the deletion."
    (when (or (= (char-before) ?<)
              (= (char-before) ?>))
      (let ((node-parent (save-excursion
                           (backward-char)
                           (treesit-node-type
                            (treesit-node-parent
                             (treesit-node-at (point)))))))
        (when (string= node-parent "type_argument_list")
          (let ((table (copy-syntax-table csharp-mode-syntax-table)))
            (modify-syntax-entry ?< "(>" table)
            (modify-syntax-entry ?> ")>" table)
            (with-syntax-table table
              (if (= (char-before) ?<)
                  (backward-char)
                (backward-sexp))
              (funcall f)))
          t))))

  ;; TODO: include only my modifications rather than the whole data structure
  (setopt csharp-ts-mode--indent-rules
          '((c-sharp
             ((parent-is "compilation_unit") parent-bol 0)
             ((node-is "}") parent-bol 0)
             ((node-is ")") parent-bol 0)
             ((node-is "]") parent-bol 0)
             ((and (parent-is "comment") c-ts-common-looking-at-star)
              c-ts-common-comment-start-after-first-star -1)
             ((parent-is "comment") prev-adaptive-prefix 0)
             ((parent-is "namespace_declaration") parent-bol 0)
             ((parent-is "class_declaration") parent-bol 0)
             ((parent-is "constructor_declaration") parent-bol 0)
             ((parent-is "initializer_expression") parent-bol csharp-ts-mode-indent-offset)
             ((match "{" "anonymous_object_creation_expression") parent-bol 0)
             ((parent-is "anonymous_object_creation_expression") parent-bol csharp-ts-mode-indent-offset)
             ((match "{" "object_creation_expression") parent-bol 0)
             ((parent-is "object_creation_expression") parent-bol 0)
             ((parent-is "method_declaration") parent-bol 0)
             ((parent-is "enum_declaration") parent-bol 0)
             ((parent-is "operator_declaration") parent-bol 0)
             ((parent-is "field_declaration") parent-bol 0)
             ((parent-is "struct_declaration") parent-bol 0)
             ((parent-is "declaration_list") parent-bol csharp-ts-mode-indent-offset)
             ((parent-is "argument_list") parent-bol csharp-ts-mode-indent-offset)
             ((parent-is "interpolation") parent-bol csharp-ts-mode-indent-offset)
             ((parent-is "binary_expression") parent 0)
             ((parent-is "block") parent-bol csharp-ts-mode-indent-offset)
             ((parent-is "local_function_statement") parent-bol 0)
             ((match "block" "if_statement") parent-bol 0)
             ((match "else" "if_statement") parent-bol 0)
             ((parent-is "if_statement") parent-bol csharp-ts-mode-indent-offset)
             ((parent-is "for_statement") parent-bol 0)
             ((parent-is "for_each_statement") parent-bol 0)
             ((parent-is "while_statement") parent-bol 0)
             ((match "{" "switch_expression") parent-bol 0)
             ((parent-is "switch_statement") parent-bol 0)
             ((parent-is "switch_body") parent-bol csharp-ts-mode-indent-offset)
             ((parent-is "switch_section") parent-bol csharp-ts-mode-indent-offset)
             ((parent-is "switch_expression") parent-bol csharp-ts-mode-indent-offset)
             ((parent-is "case_statement") parent-bol 0)
             ((parent-is "do_statement") parent-bol 0)
             ((parent-is "equals_value_clause") parent-bol csharp-ts-mode-indent-offset)
             ((parent-is "ternary_expression") parent-bol csharp-ts-mode-indent-offset)
             ((parent-is "conditional_expression") parent-bol csharp-ts-mode-indent-offset)
             ((parent-is "statement_block") parent-bol csharp-ts-mode-indent-offset)
             ((parent-is "type_arguments") parent-bol csharp-ts-mode-indent-offset)
             ((parent-is "variable_declarator") parent-bol csharp-ts-mode-indent-offset)
             ((parent-is "arguments") parent-bol csharp-ts-mode-indent-offset)
             ((parent-is "array") parent-bol csharp-ts-mode-indent-offset)
             ((parent-is "formal_parameters") parent-bol csharp-ts-mode-indent-offset)
             ((parent-is "template_substitution") parent-bol csharp-ts-mode-indent-offset)
             ((parent-is "object_pattern") parent-bol csharp-ts-mode-indent-offset)
             ((parent-is "object") parent-bol csharp-ts-mode-indent-offset)
             ((parent-is "object_type") parent-bol csharp-ts-mode-indent-offset)
             ((parent-is "enum_body") parent-bol csharp-ts-mode-indent-offset)
             ((parent-is "arrow_function") parent-bol csharp-ts-mode-indent-offset)
             ((parent-is "parenthesized_expression") parent-bol csharp-ts-mode-indent-offset)
             ;; what i have added
             ((parent-is "parameter_list") parent-bol csharp-ts-mode-indent-offset)
             ((parent-is "implicit_parameter_list") parent-bol csharp-ts-mode-indent-offset)
             ((parent-is "member_access_expression") parent-bol csharp-ts-mode-indent-offset)

             ((match "block" "lambda_expression") parent-bol 0)
             ((parent-is "lambda_expression") parent-bol csharp-ts-mode-indent-offset)

             ((parent-is "try_statement") parent-bol 0)
             ((parent-is "catch_clause") parent-bol 0)
             ((parent-is "record_declaration") parent-bol 0)
             ((parent-is "interface_declaration") parent-bol 0)
             ((parent-is "throw_expression") parent-bol csharp-ts-mode-indent-offset)
             ((parent-is "return_statement") parent-bol csharp-ts-mode-indent-offset)
             ((parent-is "record_declaration") parent-bol 0)
             ((parent-is "interface_declaration") parent-bol 0)
             ((parent-is "arrow_expression_clause") parent-bol csharp-ts-mode-indent-offset)
             ((parent-is "property_pattern_clause") parent-bol csharp-ts-mode-indent-offset))))

  ;; TODO: merge into emacs core
  (nconc csharp-ts-mode--font-lock-settings
         (treesit-font-lock-rules
          :language 'c-sharp
          :feature 'property
          :override t
          `((property_declaration
             type: (generic_name name: (identifier) @font-lock-type-face)
             name: (identifier) @font-lock-variable-name-face))))

  (add-to-list 'compilation-error-regexp-alist-alist
               '(jacob-dotnet-stacktrace-re
                 "   at [^
]+ in \\(.+\\):line \\([[:digit:]]+\\)"
                 1
                 2))

  (add-to-list 'compilation-error-regexp-alist 'jacob-dotnet-stacktrace-re)

  (jacob-defhookf csharp-ts-mode-hook
    (setq treesit-defun-type-regexp "\\(method\\|constructor\\|field\\)_declaration")
    (setq jacob-backspace-function #'jacob-backspace-csharp)
    (eglot-ensure))

  (define-auto-insert "\\.cs$" ["template.cs" jacob-autoinsert-yas-expand])
  (define-auto-insert "Controller\\.cs$" ["controllerTemplate.cs" jacob-autoinsert-yas-expand])

  (defun eglot-csharp-ls-metadata (xrefs)
    "Advice for `eglot--lsp-xrefs-for-method'.

- For showing `csharp-ls' metadata in Emacs.

- Check the first xref from XREFS to see if it's referring to a
  non-existant metadata file.

- If so, create it in a “sensible” location and modify the xref to
  point there."
    (dolist (xref-match-item xrefs)
      (when-let ((xref-file-location (xref-item-location xref-match-item))
                 (uri (xref-file-location-file xref-file-location))
                 (uri-path (when (string-match "^csharp:\\(.*\\)$" uri)
                             (match-string 1 uri)))
                 (target-path (concat (directory-file-name (project-root (project-current)))
                                      uri-path))
                 (source (plist-get (eglot--request (eglot-current-server)
                                                    :csharp/metadata
                                                    `(:textDocument (:uri ,uri)))
                                    :source)))
        (with-temp-buffer
          (insert source)
          (write-file target-path nil))

        (setf (xref-file-location-file xref-file-location)
              target-path)))
    xrefs)

  (advice-add #'eglot--lsp-xrefs-for-method :filter-return #'eglot-csharp-ls-metadata)

  (add-to-list 'auto-mode-alist '("\\.csx\\'". csharp-ts-mode)))

(require 'jacob-sharper)

(use-package csproj-mode
  :ensure t
  :mode ("\\.csproj\\'" . csproj-mode))

(use-package font-lock-ext ; dependency of `sln-mode'
  :vc ( :url "https://github.com/sensorflo/font-lock-ext.git"
        :rev :newest)
  :defer t)

;; TODO: package `sln-mode' for elpa/melpa?
(use-package sln-mode
  :vc ( :url "https://github.com/sensorflo/sln-mode.git"
        :rev :newest)
  :mode ("\\.sln\\'" . sln-mode))

(use-package fsharp-mode
  :defer t
  :mode ("\\.fs\\'" . fsharp-mode)
  :config
  (remove-hook 'project-find-functions #'fsharp-mode-project-root)
  (setopt compilation-error-regexp-alist (remq 'fsharp compilation-error-regexp-alist)))

(use-package ls-lisp
  :defer t
  :init
  (setq ls-lisp-use-insert-directory-program nil
        ls-lisp-dirs-first t))

(use-package dired
  :defer t
  :init
  (add-hook 'dired-mode-hook #'dired-hide-details-mode)
  (add-hook 'dired-mode-hook #'auto-revert-mode)

  (with-eval-after-load "xah-fly-keys"
    (jacob-xfk-bind-for-mode dired-mode
                             "s" #'dired-find-file
                             "d" #'dired-do-delete                                        ; we skip the "flag, delete" process as files are sent to system bin on deletion
                             "q" #'quit-window
                             "i" #'dired-previous-line
                             "k" #'dired-next-line
                             "e" #'dired-mark
                             "r" #'dired-unmark
                             "g" #'revert-buffer
                             "x" #'dired-do-rename
                             "c" #'dired-do-copy
                             "u" #'dired-up-directory
                             "j" #'dired-goto-file))
  :config
  (setopt dired-recursive-copies 'always
          dired-dwim-target t
          dired-listing-switches "-hal" ; the h option needs to come first 🙃
          dired-guess-shell-alist-user '(("\\.mkv\\'" "mpv")
                                         ("\\.mp4\\'" "mpv"))))
(use-package dired-aux
  :defer t
  :after dired
  :config
  (setopt dired-vc-rename-file t))

(use-package dired-rsync
  :ensure t
  :defer t
  :after dired
  :config
  (add-to-list 'mode-line-misc-info '(:eval dired-rsync-modeline-status 'append)))

(use-package esh-mode
  :defer t
  :config
  (setopt eshell-scroll-to-bottom-on-output t)

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

(use-package pcomplete
  :defer t
  :config
  (defun jacob-git-get-branches (&optional display-origin)
    "Get git branches for current repo.

Non-nil DISPLAY-ORIGIN displays whether a branch is from origin, nil
hides this information."
    (with-temp-buffer
      (insert (shell-command-to-string "git branch -a"))
      (backward-delete-char 1)            ; delete rouge newline at end
      (goto-char (point-min))
      (flush-lines "->")
      (while (re-search-forward (if display-origin
                                    "remotes/"
                                  "remotes/origin/")
                                nil
                                t)
        (replace-match ""))
      (delete-rectangle (point-min) (progn
                                      (goto-char (point-max))
                                      (+ 2 (line-beginning-position))))
      (split-string (buffer-substring-no-properties (point-min) (point-max)) "\n")))
  
  (defun pcomplete/gco ()
    "Completion for the gco alias on git branches."
    (pcomplete-here* (jacob-git-get-branches)))

  (defun pcomplete/grh ()
    "Completion for the grh alias on git branches."
    (pcomplete-here* (jacob-git-get-branches t))))

(use-package eldoc
  :hook (prog-mode-hook . global-eldoc-mode)
  :delight eldoc-mode
  :config
  (setopt eldoc-documentation-strategy 'eldoc-documentation-compose))

(use-package project
  :defer t
  :config
  (setopt project-switch-commands '((project-find-file "Find file")
                                    (jacob-project-search "Find regexp")
                                    (project-find-dir "Find directory")
                                    (magit-project-status "Version Control")
                                    (project-eshell "Shell")
                                    (project-compile "Compile"))))

(use-package prodigy
  :ensure t
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

  (jacob-xfk-bind-for-mode prodigy-mode
                           "d" #'prodigy-stop
                           "e" #'prodigy-mark
                           "g" #'jacob-project-search
                           "f" #'project-find-file
                           "i" #'prodigy-prev
                           "k" #'prodigy-next
                           "q" #'quit-window
                           "r" #'prodigy-unmark
                           "s" #'prodigy-restart
                           "v" #'prodigy-display-process)

  (add-hook 'prodigy-view-mode-hook #'compilation-minor-mode)

  (jacob-xfk-bind-for-mode prodigy-view-mode
                           "q" #'quit-window
                           "g" #'prodigy-restart))

(use-package hi-lock
  :config
  (delight 'hi-lock-mode nil t))

(use-package highlight-defined
  :ensure t
  :hook (emacs-lisp-mode-hook . highlight-defined-mode))

(use-package hl-todo
  :ensure t
  :hook (after-init-hook . global-hl-todo-mode))

(use-package lisp-extra-font-lock
  :ensure t
  :hook ((emacs-lisp-mode-hook lisp-mode-hook scheme-mode-hook) . lisp-extra-font-lock-mode))

(use-package elisp-mode
  :defer t
  :config
  (defun jacob-move-past-close-and-reindent ()
    "Advice for `move-past-close-and-reindent'."
    (when (bolp)
      (delete-blank-lines)))

  (advice-add #'move-past-close-and-reindent :after #'jacob-move-past-close-and-reindent)

  (defun jacob-indent-buffer ()
    "Indent whole buffer.  Designed for use in `before-save-hook'."
    (unless (ignore-errors smerge-mode)
      (indent-region (point-min) (point-max))))

  (jacob-defhookf emacs-lisp-mode-hook
    (flymake-mode 1)
    (add-hook 'before-save-hook 'jacob-indent-buffer nil "LOCAL")
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

  (setopt elisp-flymake-byte-compile-load-path load-path))

(use-package scheme
  :mode ("\\.scm\\'" . scheme-mode)
  :config
  (keymap-set scheme-mode-map "(" #'insert-parentheses)
  (keymap-set scheme-mode-map ")" #'move-past-close-and-reindent))

(use-package geiser
  :ensure t
  :after scheme)

(use-package geiser-mode
  :after (scheme geiser)
  :config
  (jacob-xfk-bind-for-mode geiser-mode
                           "SPC , m" #'geiser-eval-last-sexp
                           "SPC , d" #'geiser-eval-definition))

(use-package geiser-guile
  :ensure t
  :after (scheme geiser))

(use-package mermaid-mode
  :ensure t
  :mode ("\\.mermaid\\'"))

(use-package ob-mermaid
  :ensure t
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

  ;; stolen from doom
  (with-no-warnings
    (custom-declare-face '+org-todo-active  '((t (:inherit (bold font-lock-constant-face org-todo)))) "")
    (custom-declare-face '+org-todo-project '((t (:inherit (bold font-lock-doc-face org-todo)))) "")
    (custom-declare-face '+org-todo-onhold  '((t (:inherit (bold warning org-todo)))) "")
    (custom-declare-face '+org-todo-cancel  '((t (:inherit (bold font-lock-comment-face org-todo)))) ""))

  (setopt org-startup-folded t
          org-tags-column 0
          org-capture-templates '(("i" "Inbox" entry (file "") "* TODO %?\n:PROPERTIES:\n:CREATED: %U\n:END:"))
          org-todo-keywords '((sequence
                               "TODO(t)"  ; A task that needs doing & is ready to do
                               "PROJ(p)"  ; A project, which usually contains other tasks
                               "LOOP(r)"  ; A recurring task
                               "STRT(s)"  ; A task that is in progress
                               "WAIT(w)"  ; Something external is holding up this task
                               "HOLD(h)"  ; This task is paused/on hold because of me
                               "IDEA(i)"  ; An unconfirmed and unapproved task or notion
                               "|"
                               "DONE(d)"  ; Task successfully completed
                               "KILL(k)") ; Task was cancelled, aborted, or is no longer applicable
                              (sequence
                               "[ ](T)"   ; A task that needs doing
                               "[-](S)"   ; Task is in progress
                               "[?](W)"   ; Task is being held up or paused
                               "|"
                               "[X](D)")  ; Task was completed
                              (sequence
                               "|"
                               "OKAY(o)"
                               "YES(y)"
                               "NO(n)"))
          org-todo-keyword-faces '(("[-]"  . +org-todo-active)
                                   ("STRT" . +org-todo-active)
                                   ("[?]"  . +org-todo-onhold)
                                   ("WAIT" . +org-todo-onhold)
                                   ("HOLD" . +org-todo-onhold)
                                   ("PROJ" . +org-todo-project)
                                   ("NO"   . +org-todo-cancel)
                                   ("KILL" . +org-todo-cancel)))

  (jacob-defhookf org-mode-hook
    (visual-fill-column-mode 1)
    (toggle-truncate-lines 0)
    (toggle-word-wrap 1)))

(use-package org-agenda
  :commands (org-agenda org-capture)
  :init
  (defvar-keymap jacob-org-agenda-map
    "a" #'org-agenda
    "c" #'org-capture)

  (with-eval-after-load "xah-fly-keys"
    (keymap-set jacob-xfk-map "a" `("Agenda" . ,jacob-org-agenda-map)))
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

  (jacob-xfk-bind-for-mode org-agenda-mode
                           "q" #'quit-window
                           "g" #'org-agenda-redo-all))

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

(use-package org-edna
  :ensure t
  :after org
  :delight
  :config
  (org-edna-mode 1))

(use-package denote
  :ensure t
  :defer t)

(use-package pulse
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
  :hook (on-first-file-hook . server-start))

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
  (jacob-xfk-bind-for-mode calendar-mode
                           "q" #'quit-window
                           "i" #'calendar-backward-week
                           "k" #'calendar-forward-week
                           "j" #'calendar-backward-day
                           "l" #'calendar-forward-day
                           "u" #'calendar-backward-month
                           "o" #'calendar-forward-month
                           "d" #'diary-view-entries
                           "s" #'diary-insert-entry
                           "m" #'diary-mark-entries
                           "." #'calendar-goto-today
                           "t" #'calendar-set-mark)

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
  :hook (on-first-input-hook . winner-mode)
  :init
  (with-eval-after-load "xah-fly-keys"
    (keymap-set xah-fly-command-map "1" #'winner-undo)
    (keymap-set xah-fly-command-map "2" #'winner-redo)))

(use-package compile
  :defer t
  :init
  (keymap-global-set "<f5>" 'recompile)
  :config
  (jacob-xfk-bind-for-mode compilation-mode
                           "g" #'recompile
                           "q" #'quit-window)

  (add-hook 'compilation-filter-hook #'ansi-color-compilation-filter)

  (setopt compilation-always-kill t
          compilation-scroll-output t))

(use-package winnow
  :ensure t
  :hook (compilation-mode-hook . winnow-mode))

(use-package sql
  :commands (sql-read-connection)
  :init
  (defun jacob-sql-connect ()
    "Wrapper for `sql-connect' to set postgres password.
CONNECTION is the connection settings."
    (interactive)
    (let ((connection (sql-read-connection "Connection: ")))
      (with-environment-variables
          (("PGPASSWORD" (cadr (assoc 'sql-password
                                      (assoc-string connection
                                                    sql-connection-alist
                                                    t)))))
        (sql-connect connection))))

  (with-eval-after-load "xah-fly-keys"
    (keymap-set jacob-xfk-map "s" #'jacob-sql-connect))
  :config
  (jacob-defhookf sql-interactive-mode-hook
    (when (eq sql-product 'postgres)
      (setq sql-prompt-regexp "^[-[:alnum:]_]*[-=]\\*?[#>] ")
      (setq sql-prompt-cont-regexp "^\\(?:\\sw\\|\\s_\\)*[-(]\\*?[#>] ")))

  (jacob-xfk-bind-for-mode sql-interactive-mode
                           "SPC , d" #'sql-send-paragraph)

  (defun jacob-sqli-end-of-buffer ()
    "Move point to end of sqli buffer before sending paragraph.

Intended as before advice for `sql-send-paragraph'."
    (with-current-buffer sql-buffer
      (goto-char (point-max))))

  (advice-add #'sql-send-paragraph :before #'jacob-sqli-end-of-buffer))

(use-package doc-view
  :defer t
  :config
  (jacob-xfk-bind-for-mode doc-view-mode
                           "l" 'doc-view-next-page
                           "j" 'doc-view-previous-page))

(use-package treesit
  :defer t
  :config
  (setopt treesit-font-lock-level 4))

(use-package treesit-auto
  :ensure t
  :hook (prog-mode-hook . global-treesit-auto-mode)
  :config
  (add-to-list 'treesit-auto-recipe-list (make-treesit-auto-recipe
                                          :lang 'gdscript
                                          :ts-mode 'gdscript-ts-mode
                                          :remap 'gdscript-mode
                                          :url "https://github.com/PrestonKnopp/tree-sitter-gdscript.git"
                                          :ext "\\.gd\\'"))

  (add-to-list 'treesit-auto-langs 'gdscript)
  
  (treesit-auto-add-to-auto-mode-alist))

(use-package typescript-ts-mode
  :mode ("\\.ts" . typescript-ts-mode)
  :config
  (jacob-defhookf typescript-ts-mode-hook
    (setq-local forward-sexp-function nil)
    (setq-local transpose-sexps-function nil)))

(use-package yaml-ts-mode
  :mode ("\\.ya?ml\\'" . yaml-ts-mode))

(use-package message
  :after gnus
  :config
  (jacob-defhookf message-mode-hook
    (setq-local auto-save-visited-mode nil))
  (setopt message-send-mail-function 'smtpmail-send-it))

(use-package nxml-mode
  :defer t
  :init
  (add-to-list 'auto-mode-alist '("Directory.Packages.props" . nxml-mode)))

(use-package key-chord
  :ensure t
  :config
  (key-chord-mode 1))

(use-package avy
  :ensure t
  :defer t
  :init
  (key-chord-define-global "fj" #'avy-goto-char-timer)
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

(use-package apheleia
  :ensure t
  :delight '(:eval (if apheleia-inhibit "" " ⚘"))
  :hook (prog-mode-hook . apheleia-global-mode)
  :config
  (setq-default apheleia-inhibit t) ; set `apheleia-inhibit' to nil to enable
  (add-to-list 'apheleia-formatters '(csharpier "dotnet" "csharpier" "--write-stdout"))
  (add-to-list 'apheleia-mode-alist '(csharp-ts-mode . csharpier))

  (defun jacob-apheleia-skip-function ()
    "Function for `apheleia-skip-functions'.
If point is in a yasnippet field or the minibuffer or region are
active, do not format the buffer."
    (or (seq-find (lambda (overlay)
                    (overlay-get overlay 'yas--snippet))
                  (overlays-at (point)))
        (minibuffer-window-active-p (car (window-list)))
        (region-active-p)))

  (add-to-list 'apheleia-skip-functions #'jacob-apheleia-skip-function))

(use-package rainbow-mode
  :ensure t
  :delight rainbow-mode
  :hook (on-first-file-hook . rainbow-mode))

(use-package eglot-booster
  :after eglot
  :when (executable-find "emacs-lsp-booster")
  :vc ( :url "https://github.com/jdtsmith/eglot-booster"
        :rev :newest)
  :config
  (eglot-booster-mode 1))

(use-package dape
  :ensure t
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

(use-package csharp-toolbox
  :vc ( :url "https://github.com/lem102/csharp-toolbox.git"
        :rev :newest)
  :after csharp-mode
  :config
  ;; (keymap-set jacob-xfk-map "c f" #'csharp-toolbox-format-statement)
  ;; (keymap-set jacob-xfk-map "c t" #'csharp-toolbox-run-test)
  ;; (keymap-set jacob-xfk-map "c a" #'csharp-toolbox-toggle-async)
  ;; (keymap-set jacob-xfk-map "c n" #'csharp-toolbox-guess-namespace)
  ;; (keymap-set jacob-xfk-map "c ;" #'csharp-toolbox-wd40)
  )

(use-package ace-window
  :ensure t
  :defer t
  :init
  (defun jacob-split-or-switch-window ()
    "Split or switch window.

If there is only one window in the current frame, split the frame and
move to the new window. Otherwise, call `switch-buffer'."
    (interactive)
    (cond ((= 1 (let ((total-windows 0))
                  (dolist (frame (frame-list))
                    (setq total-windows (+ total-windows (length (window-list frame)))))
                  total-windows))
           (split-window-sensibly)
           (call-interactively #'other-window))
          (t (call-interactively #'ace-window))))

  (with-eval-after-load "xah-fly-keys"
    (keymap-set xah-fly-command-map "," #'jacob-split-or-switch-window))
  :config
  (setopt aw-keys '(?a ?s ?d ?f ?q ?w ?e ?r)
          aw-minibuffer-flag t))

(use-package tex
  :ensure auctex
  :commands (TeX-PDF-mode)
  :mode ("\\.tex\\'" . latex-mode)
  :config
  (add-hook 'LaTeX-mode-hook #'visual-fill-column-mode 1)
  (add-hook 'LaTeX-mode-hook #'toggle-word-wrap 1)
  (add-hook 'LaTeX-mode-hook #'TeX-PDF-mode 1)
  :custom ((TeX-auto-save t)
           (TeX-parse-self t)
           (japanese-TeX-error-messages nil)))

(use-package visual-fill-column
  :ensure t
  :hook ((org-mode-hook LaTeX-mode-hook) . visual-fill-column-mode))

(use-package markdown-mode
  :ensure t
  :defer t)

(use-package feature-mode
  :ensure t
  :defer t)

(use-package vertico
  :ensure t
  :hook (jacob-first-minibuffer-activation-hook . vertico-mode))

(use-package vertico-mouse
  :hook (vertico-mode-hook . vertico-mouse-mode))

(use-package orderless
  :ensure t
  :preface
  (defun jacob-load-orderless ()
    "Load the `orderless' library."
    (require 'orderless))
  :hook (jacob-first-minibuffer-activation-hook . jacob-load-orderless)
  :config
  (setopt completion-styles '(orderless initials)))

(use-package marginalia
  :ensure t
  :hook (jacob-first-minibuffer-activation-hook . marginalia-mode))

(use-package consult
  :ensure t
  :defer t
  :init
  (with-eval-after-load "xah-fly-keys"
    (keymap-set xah-fly-leader-key-map "v" #'consult-yank-from-kill-ring)
    (keymap-set xah-fly-leader-key-map "f" #'consult-buffer)
    (keymap-set xah-fly-leader-key-map "i j" #'consult-recent-file)
    (keymap-set xah-fly-leader-key-map "e s" #'consult-line)
    (keymap-set xah-fly-leader-key-map "k u" #'consult-goto-line))

  (keymap-global-set "C-x b" #'consult-buffer)
  (keymap-global-set "M-y" #'consult-yank-from-kill-ring)
  :config
  (defun jacob-project-search ()
    "Wrapper for grep commands."
    (interactive)
    (if (vc-find-root default-directory ".git")
        (consult-git-grep)
      (consult-grep)))

  (defun jacob-consult-buffer-state-no-tramp ()
    "Buffer state function that doesn't preview Tramp buffers."
    (let ((orig-state (consult--buffer-state))
          (filter (lambda (action candidate)
                    (if (and candidate
                             (or (eq action 'return)
                                 (let ((buffer (get-buffer candidate)))
                                   (and buffer
                                        (not (file-remote-p (buffer-local-value 'default-directory buffer)))))))
                        candidate
                      nil))))
      (lambda (action candidate)
        (funcall orig-state action (funcall filter action candidate)))))

  (setopt completion-in-region-function 'consult-completion-in-region
          xref-show-xrefs-function 'consult-xref
          xref-show-definitions-function 'consult-xref
          consult--source-buffer (plist-put consult--source-buffer
                                            :state #'jacob-consult-buffer-state-no-tramp))

  (keymap-set project-prefix-map "g" #'jacob-project-search))

(use-package consult-imenu
  :defer t
  :init
  (keymap-global-set "M-g i" #'consult-imenu))

(use-package embark
  :ensure t
  :defer t
  :init
  (with-eval-after-load "xah-fly-keys"
    (keymap-set xah-fly-command-map "\\" #'embark-act)

    (jacob-xfk-bind-for-mode embark-collect
                             "q" #'quit-window))
  :config
  (setopt embark-cycle-key "\\"
          embark-help-key "h")

  (keymap-set embark-flymake-map "a" #'eglot-code-actions)
  (push 'embark--ignore-target (alist-get 'eglot-code-actions embark-target-injection-hooks))

  (keymap-set embark-identifier-map "r" #'eglot-rename)
  (push 'embark--ignore-target (alist-get 'eglot-rename embark-target-injection-hooks)))

(use-package embark-consult
  :ensure t
  :after (:and embark consult))

(use-package expreg
  :ensure t
  :defer t
  :init
  (keymap-global-set "C-c SPC" #'expreg-expand)
  
  (defvar-keymap jacob-expreg-repeat-map
    :repeat t
    "SPC" #'expreg-expand)

  (with-eval-after-load "xah-fly-keys"
    (keymap-set xah-fly-command-map "8" #'expreg-expand)
    (keymap-set xah-fly-command-map "9" #'expreg-contract)))

(use-package verb
  :ensure t
  :after org
  :hook (org-mode-hook . verb-mode)
  :config
  (jacob-xfk-bind-for-mode verb-response-body-mode
                           "q" #'quit-window)

  (defun jacob-verb-id (response-id)
    "Get the id property from the stored verb response pertaining to RESPONSE-ID."
    (verb-json-get (oref (verb-stored-response response-id) body) "id")))

(use-package sly
  :ensure t
  :hook (lisp-mode-hook . sly-mode)
  :config
  (sly-setup)

  (sly-symbol-completion-mode 0)

  (jacob-xfk-bind-for-mode sly-mode
                           "SPC , m" #'sly-eval-last-expression
                           "SPC , d" #'sly-eval-defun
                           "SPC , e" #'sly-eval-buffer
                           "SPC w k" #'sly-edit-definition)

  (setopt sly-auto-start 'always
          inferior-lisp-program "sbcl")

  (jacob-xfk-bind-for-mode sly-db
                           "q" #'sly-db-quit))

(use-package sly-overlay
  :ensure t
  :after sly)

(use-package sly-macrostep
  :ensure t
  :after sly)

;; (jacob-require sly-stepper "https://github.com/joaotavora/sly-stepper.git")

(use-package sly-quicklisp
  :ensure t
  :after sly)

(use-package sql-indent
  :ensure t
  :hook (sql-mode-hook . sqlind-minor-mode))

(use-package gptel
  :ensure t
  :defer t
  :config
  (setopt gptel-default-mode #'org-mode
          gptel-confirm-tool-calls t)

  (gptel-make-tool :name "variable_completions"
                   :function (lambda (query)
                               (let (symbols)
                                 (mapatoms (lambda (symbol)
                                             (let ((name (symbol-name symbol)))
                                               (when (and (boundp symbol)
                                                          (string-match-p query name))
                                                 (push symbol symbols)))))
                                 symbols))
                   :description "get the names of all the variables that match the search query"
                   :args (list '( :name "search query"
                                  :type string
                                  :description "the search query"))
                   :category "emacs")

  (gptel-make-tool :name "function_completions"
                   :function (lambda (query)
                               (let (symbols)
                                 (mapatoms (lambda (symbol)
                                             (let ((name (symbol-name symbol)))
                                               (when (and (fboundp symbol)
                                                          (string-match-p query name))
                                                 (push symbol symbols)))))
                                 symbols))
                   :description "get the names of all the functions that match the search query"
                   :args (list '( :name "search query"
                                  :type string
                                  :description "the search query"))
                   :category "emacs")

  (gptel-make-tool :name "command_completions"
                   :function (lambda (query)
                               (let (symbols)
                                 (mapatoms (lambda (symbol)
                                             (let ((name (symbol-name symbol)))
                                               (when (and (commandp symbol)
                                                          (string-match-p query name))
                                                 (push symbol symbols)))))
                                 symbols))
                   :description "get the names of all the commands that match the search query"
                   :args (list '( :name "search query"
                                  :type string
                                  :description "the search query"))
                   :category "emacs")

  (gptel-make-tool :name "variable_documentation"
                   :function (lambda (variable-name)
                               (let ((symbol (intern-soft variable-name)))
                                 (when (and symbol (boundp symbol))
                                   (documentation-property symbol
                                                           'variable-documentation))))
                   :description "get the documentation for an emacs variable"
                   :args (list '( :name "variable name"
                                  :type string
                                  :description "the variable name"))
                   :category "emacs")

  (gptel-make-tool :name "function_documentation"
                   :function (lambda (function-name)
                               (let ((symbol (intern-soft function-name)))
                                 (when (and symbol (fboundp symbol))
                                   (documentation symbol))))
                   :description "get the documentation for an emacs function"
                   :args (list '( :name "function name"
                                  :type string
                                  :description "the function name"))
                   :category "emacs")

  (gptel-make-tool :name "variable_value"
                   :function (lambda (variable-name)
                               (let ((symbol (intern-soft variable-name)))
                                 (when (and symbol (boundp symbol))
                                   (symbol-value symbol))))
                   :description "get the value of an emacs variable"
                   :args (list '( :name "variable name"
                                  :type string
                                  :description "the variable's name"))
                   :category "emacs")

  (gptel-make-tool :name "variable_source"
                   :function (lambda (variable-name)
                               (let ((symbol (intern-soft variable-name)))
                                 (when (and symbol (boundp symbol))
                                   (with-temp-buffer
                                     (find-variable symbol)
                                     (buffer-substring-no-properties (point)
                                                                     (save-excursion
                                                                       (end-of-defun)
                                                                       (point)))))))
                   :description "get the source code of an emacs variable"
                   :args (list '( :name "variable name"
                                  :type string
                                  :description "the variable's name"))
                   :category "emacs")

  (defun jacob-gptel-function-source (function-name)
    "Get the source code of an Emacs function called FUNCTION-NAME."
    (let ((symbol (intern-soft function-name)))
      (when (and symbol (fboundp symbol))
        (save-window-excursion
          (find-function symbol)
          (buffer-substring-no-properties (point)
                                          (save-excursion
                                            (end-of-defun)
                                            (point)))))))

  (gptel-make-tool :name "function_source"
                   :function #'jacob-gptel-function-source
                   :description "get the source code of an emacs function"
                   :args (list '( :name "function name"
                                  :type string
                                  :description "the function's name"))
                   :category "emacs")

  (defun jacob-gptel-symbol-manual-section (symbol-name)
    "Get the manual node for SYMBOL-NAME."
    (when-let ((symbol (intern-soft symbol-name)))
      (save-window-excursion
        (info-lookup-symbol symbol)
        (buffer-string))))

  (gptel-make-tool :name "symbol_manual_section"
                   :function #'jacob-gptel-symbol-manual-section
                   :description "get the manual page of an emacs symbol"
                   :args (list '( :name "manual page name"
                                  :type string
                                  :description "the name of the manual page"))
                   :category "emacs")

  (defun jacob-gptel-library-source (library-name)
    "Get the source code of LIBRARY-NAME."
    (when (locate-library library-name)
      (save-window-excursion
        (find-library library-name)
        (buffer-string))))

  (gptel-make-tool :name "library_source"
                   :function #'jacob-gptel-library-source
                   :description "get the source code of a library or package in emacs"
                   :args (list '( :name "library name"
                                  :type string
                                  :description "the library name"))
                   :category "emacs")

  (defun jacob-gptel-manual-node-contents (node-name)
    "Get the contents of the manul node NODE-NAME."
    (save-window-excursion
      (Info-goto-node node-name)
      (buffer-string)))

  (gptel-make-tool :name "manual_node_contents"
                   :function #'jacob-gptel-manual-node-contents
                   :description "get the contents of the manual node."
                   :args (list '( :name "manual node name"
                                  :type string
                                  :description "the manual node's name"))
                   :category "emacs"))

;; manual nodes

(defun jacob-gptel-manual-nodes (manual)
  "Get all nodes in MANUAL."
  (info--manual-names nil))

;; manual names

(defun jacob-gptel-manual-names ()
  "Get a list of all info manual names."
  (info--manual-names nil))

;; features

;; load paths

;; symbol exists

;; elisp eval ?

(use-package aider
  :ensure t
  :defer t
  ;; (setenv "GEMINI_API_KEY" "")
  :custom ((aider-args '("--model" "gemini/gemini-2.0-flash-exp"))))

(use-package gdscript-mode
  :ensure t
  :mode ("\\.gd\\'" . gdscript-ts-mode)
  :config
  (add-hook 'gdscript-ts-mode-hook #'indent-tabs-mode))

(use-package eat
  :ensure t
  :when jacob-is-linux
  :defer t
  :init
  (add-hook 'eshell-mode-hook #'eat-eshell-mode))

(use-package pdf-tools
  :ensure t
  :when jacob-is-linux
  :mode ("\\.[pP][dD][fF]\\'" . pdf-view-mode))

(use-package grep
  :when jacob-is-windows
  :defer t
  :config
  (setopt find-program "C:/Program Files (x86)/GnuWin32/bin/find.exe"))

(use-package just-mode
  :ensure t
  :defer t)


;; personal functions

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

(provide 'init)

;;; init.el ends here
