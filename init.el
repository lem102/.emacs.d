;;; init.el --- Jacob's main init file. -*-lexical-binding: t-*-
;;; Commentary:
;;; Code:

(use-package no-littering
  :ensure t
  :demand)

;; use-package
(require 'use-package)

(setopt use-package-enable-imenu-support t
        use-package-verbose t
        use-package-compute-statistics t
        use-package-hook-name-suffix nil)

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

(defvar jacob-first-file-hook '()
  "Hook for first file opened.")

(defun jacob-run-first-file-hook (&rest _args)
  "Run `jacob-first-file-hook', then remove this function from `find-file-hook'."
  (when (member 'init features)
    (run-hooks 'jacob-first-file-hook)
    (advice-remove #'jacob-run-first-file-hook #'create-file-buffer)))

(advice-add #'create-file-buffer :before #'jacob-run-first-file-hook)

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

  :custom (
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
           (echo-keystrokes 0.01)
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
           (tab-always-indent 'complete)))

(use-package package
  :custom (package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                              ("nongnu" . "https://elpa.nongnu.org/nongnu/")
                              ("melpa" . "https://melpa.org/packages/"))))

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

(use-package which-key
  :delight
  :config
  (which-key-mode 1)
  :custom (which-key-idle-delay 0.01))

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
  :delight)

(use-package window
  :custom
  (switch-to-buffer-obey-display-actions t)
  (display-buffer-alist '(((major-mode . sql-interactive-mode)
                           (display-buffer-reuse-mode-window display-buffer-same-window))
                          ((major-mode . prodigy-mode)
                           (display-buffer-reuse-mode-window display-buffer-same-window))
                          ((major-mode . magit-status-mode)
                           (display-buffer-reuse-mode-window display-buffer-same-window))))
  (split-height-threshold nil))

(defvar-keymap jacob-recenter-repeat-map
  :repeat t
  "p" #'recenter-top-bottom)

(use-package frame
  :custom (blink-cursor-blinks 0)     ; make cursor blink forever
  :bind ("C-z" . nil)                 ; `suspend-frame'
  )

(use-package novice
  :custom (disabled-command-function nil))

(use-package recentf
  :hook (after-init-hook . recentf-mode))

(use-package savehist
  :config
  (savehist-mode 1)
  :custom
  (savehist-save-minibuffer-history t))

(use-package saveplace
  :hook (jacob-first-file-hook . save-place-mode)
  :custom
  (save-place-forget-unreadable-files t))

(use-package custom
  :config
  (load-theme 'modus-vivendi-tinted))

(use-package tab-bar
  :custom
  (tab-bar-show 1))

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
  :config
  (setopt bookmark-set-fringe-mark nil
          bookmark-watch-bookmark-file 'silent))

(use-package flymake
  :defer t
  :init
  (keymap-global-set "M-n" #'flymake-goto-next-error)
  (keymap-global-set "M-p" #'flymake-goto-prev-error))

(use-package xah-fly-keys
  :ensure t
  :hook (after-init-hook . xah-fly-keys)
  :delight (xah-fly-keys " 🛪")
  :init
  (defvar-keymap jacob-xfk-map)

  ;; must be set before requiring `xah-fly-keys'
  (setopt xah-fly-use-control-key nil
          xah-fly-use-meta-key nil)
  :config

  (defun jacob-xfk-local-key (key command)
    "Bind KEY buffer locally to COMMAND in xfk command mode."
    (let ((existing-command (keymap-lookup xah-fly-command-map key nil "NO-REMAP")))
      (unless existing-command
        (user-error "%s is not bound to a key in `xah-fly-command-map'" key))
      (keymap-local-set (format "<remap> <%s>" existing-command)
                        command)))

  (xah-fly-keys-set-layout "qwerty")

  (defun jacob-modeline-color-on () (set-face-background 'mode-line "firebrick"))
  (defun jacob-modeline-color-off () (set-face-background 'mode-line "dark olive green"))

  (add-hook 'xah-fly-command-mode-activate-hook 'jacob-modeline-color-on)
  (add-hook 'xah-fly-insert-mode-activate-hook  'jacob-modeline-color-off)

  (keymap-set xah-fly-leader-key-map "SPC" jacob-xfk-map)
  (keymap-set jacob-xfk-map "p" `("Project" . ,project-prefix-map))

  (defvar-local jacob-backspace-function nil
    "Called by `jacob-backspace' if non-nil.")

  (defun jacob-backspace ()
    "DWIM backspace command.

  If character to the left is a pair character as determined by
  `insert-pair-alist', kill from the pair to its match.  If the
  prefix argument is provided, just delete the pair characters."
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
          (cond ((= ?\" char-class)                         ; string
                 (if (nth 3 (syntax-ppss))
                     (backward-char)
                   (backward-sexp))
                 (funcall f))
                ((= ?\( char-class)                         ; delete from start of pair
                 (backward-char)
                 (funcall f))
                ((= ?\) char-class)                         ; delete from end of pair
                 (backward-sexp)
                 (funcall f))
                (t                                          ; delete character
                 (backward-delete-char 1)))))))

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

  (defun jacob-kill-paragraph ()
    "Move to the beginning of the paragraph, then kill it."
    (interactive)
    (forward-paragraph)
    (backward-paragraph)
    (kill-paragraph 1))

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

  (defalias 'jacob-return-macro
    (kmacro "<return>"))

  (keymap-global-set "C-a" #'jacob-beginning-of-line)
  (keymap-global-set "C-e" #'jacob-end-of-line)
  (keymap-global-set "C-k" #'jacob-kill-line)
  (keymap-global-set "<backspace>" #'jacob-backspace)
  (keymap-global-set "M-;" #'xah-comment-dwim)
  (keymap-global-set "C-w" #'xah-cut-line-or-region)
  (keymap-global-set "M-w" #'xah-copy-line-or-region)

  (defvar-keymap jacob-movement-repeat-map
    :repeat t
    "n" #'next-line
    "p" #'previous-line
    "a" #'jacob-beginning-of-line
    "e" #'jacob-end-of-line
    "f" #'forward-word
    "b" #'backward-word)

  (defvar-keymap jacob-character-movement-repeat-map
    :repeat t
    "f" #'forward-char
    "b" #'backward-char)

  (defvar-keymap jacob-sexp-repeat-map
    :repeat t
    "f" #'forward-sexp
    "b" #'backward-sexp
    "n" #'forward-list
    "p" #'backward-list
    "u" #'backward-up-list
    "d" #'down-list
    "k" #'kill-sexp
    "<backspace>" #'backward-kill-sexp
    "a" #'beginning-of-defun
    "e" #'end-of-defun)

  (defvar-keymap jacob-isearch-repeat-map
    :repeat t
    "s" #'isearch-repeat-forward
    "r" #'isearch-repeat-backward)

  (keymap-global-set "<f7>" #'xah-fly-leader-key-map)
  (keymap-global-set "M-SPC" #'xah-fly-command-mode-activate)

  (keymap-set xah-fly-command-map "'" #'jacob-format-words)
  (keymap-set xah-fly-command-map "-" #'flymake-goto-prev-error)
  (keymap-set xah-fly-command-map "9" #'jacob-swap-visible-buffers)
  (keymap-set xah-fly-command-map ";" #'jacob-end-of-line)
  (keymap-set xah-fly-command-map "=" #'flymake-goto-next-error)
  (keymap-set xah-fly-command-map "d" #'jacob-backspace)
  (keymap-set xah-fly-command-map "g" #'jacob-kill-paragraph)
  (keymap-set xah-fly-command-map "h" #'jacob-beginning-of-line)
  (keymap-set xah-fly-command-map "s" #'jacob-return-macro)
  (keymap-set xah-fly-command-map "x" #'jacob-kill-line)
  (keymap-set xah-fly-command-map "," #'jacob-split-or-switch-window)

  (keymap-set xah-fly-insert-map "M-SPC" #'xah-fly-command-mode-activate)

  (keymap-set xah-fly-leader-key-map "/ b" #'vc-switch-branch)
  (keymap-set xah-fly-leader-key-map "/ c" #'vc-create-branch)
  (keymap-set xah-fly-leader-key-map "d i" #'insert-pair)
  (keymap-set xah-fly-leader-key-map "d j" #'insert-pair)
  (keymap-set xah-fly-leader-key-map "d k" #'insert-pair)
  (keymap-set xah-fly-leader-key-map "d l" #'insert-pair)
  (keymap-set xah-fly-leader-key-map "d u" #'insert-pair)
  (keymap-set xah-fly-leader-key-map "l 3" #'jacob-async-shell-command)
  (keymap-set xah-fly-leader-key-map "l a" #'global-text-scale-adjust)
  (keymap-set xah-fly-leader-key-map "w j" #'xref-find-references)
  (keymap-set xah-fly-leader-key-map ", n" #'jacob-eval-and-replace))

;; TODO: fix this haunted declaration.

;; the bodge fix is to `:demand' it.

;; not sure how to implement the keybindings i have created

;; there is a conflict with no littering i think

(defvar-keymap jacob-yas-map
  "n" #'yas-new-snippet
  "v" #'yas-visit-snippet-file)

(keymap-set jacob-xfk-map "y" `("Yasnippet" . ,jacob-yas-map))

;; tbh i'm not sure wtf is going on

(use-package yasnippet
  :ensure t
  :commands (yas-new-snippet)
  :demand                               ; needed
  ;; :custom ((yas-snippet-dirs ("/home/jacobl/.emacs.d/etc/yasnippet/snippets/")))
  ;; :bind (:map jacob-xfk-map
  ;;             ("y n" . yas-new-snippet))
  :delight yas-minor-mode
  :config

  (nbutlast yas-snippet-dirs 1)
  
  (defun jacob-point-in-text-p ()
    "Return t if in comment or string.  Else nil."
    (let ((xsyntax-state (syntax-ppss)))
      (or (nth 3 xsyntax-state)
          (nth 4 xsyntax-state))))
  
  (defun jacob-point-in-code-p ()
    "Return t if outside of string or comment.  Else nil."
    (not (jacob-point-in-text-p)))

  (jacob-defhookf snippet-mode-hook
    (setq-local auto-save-visited-mode nil))

  (yas-global-mode 1)

  (setopt yas-new-snippet-default "# -*- mode: snippet -*-
# key: $1
# --
$0`(yas-escape-text yas-selected-text)`")

  (defun jacob-autoinsert-yas-expand ()
    "Replace text in yasnippet template."
    (yas-expand-snippet (buffer-string) (point-min) (point-max)))

  (defun jacob-yas-camel-case (input)
    "Convert INPUT to camel case e.g. apple banana -> appleBanana.
For use in yasnippets."
    (let* ((space-at-end (if (string-match-p " $" input) " " ""))
           (words (split-string input))
           (capitalised-words (seq-reduce (lambda (previous current)
                                            (concat previous (capitalize current)))
                                          (cdr words)
                                          (car words))))
      (concat capitalised-words space-at-end)))

  (defun jacob-yas-pascal-case (input)
    "Convert INPUT to pascal case e.g. apple banana -> AppleBanana.
For use in yasnippets."
    (let ((space-at-end (if (string-match-p " $" input)
                            " "
                          "")))
      (with-temp-buffer
        (insert input)
        (goto-char (point-min))
        (subword-mode 1)
        (while (not (= (point) (point-max)))
          (call-interactively #'capitalize-word))
        (goto-char (point-min))
        (while (search-forward " " nil "NOERROR")
          (replace-match ""))
        (goto-char (point-max))
        (insert space-at-end)
        (buffer-substring-no-properties (point-min) (point-max)))))

  (defun jacob-yas-snake-case (input)
    "Convert INPUT to snake case e.g. apple banana -> apple_banana.
For use in yasnippets."
    (string-replace " " "_" input))

  (defun jacob-yas-kebab-case (input)
    "Convert INPUT to kebab case e.g. apple banana -> apple_banana.
For use in yasnippets."
    (string-replace " " "-" input)))

(use-package minibuffer
  :config
  (define-key minibuffer-local-completion-map "SPC" 'self-insert-command)

  (jacob-defhookf minibuffer-setup-hook
    (jacob-xfk-local-key "g" #'embark-export)))

(use-package replace
  :config
  (jacob-defhookf occur-mode-hook
    (jacob-xfk-local-key "q" 'quit-window)
    (jacob-xfk-local-key "i" 'occur-prev)
    (jacob-xfk-local-key "k" 'occur-next)))

(use-package info
  :config
  (jacob-defhookf Info-mode-hook
    (jacob-xfk-local-key "q" 'quit-window)
    (jacob-xfk-local-key "r" 'Info-scroll-up)
    (jacob-xfk-local-key "e" 'Info-scroll-down)
    (jacob-xfk-local-key "w" 'Info-up)
    (jacob-xfk-local-key "g" 'Info-menu)))

(use-package diff
  :config
  (jacob-defhookf diff-mode-hook
    (jacob-xfk-local-key "q" #'quit-window)
    (jacob-xfk-local-key "e" #'diff-hunk-prev)
    (jacob-xfk-local-key "r" #'diff-hunk-next)
    (jacob-xfk-local-key "x" #'diff-hunk-kill)
    (jacob-xfk-local-key "g" #'revert-buffer)))

(use-package help
  :config
  (setopt help-window-select t
          help-enable-variable-value-editing t))

(use-package help-fns
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
  (jacob-defhookf help-mode-hook
    (jacob-xfk-local-key "s" #'help-view-source)
    (jacob-xfk-local-key "q" #'quit-window)
    (jacob-xfk-local-key "e" #'help-go-back)
    (jacob-xfk-local-key "r" #'help-go-forward)
    (jacob-xfk-local-key "g" #'revert-buffer)
    (jacob-xfk-local-key "w" #'jacob-help-edit)))

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
  (jacob-defhookf helpful-mode-hook
    (jacob-xfk-local-key "q" #'quit-window)
    (jacob-xfk-local-key "g" #'helpful-update)
    (jacob-xfk-local-key "e" #'backward-button)
    (jacob-xfk-local-key "r" #'forward-button)
    (jacob-xfk-local-key "s" #'push-button)))

(use-package help-at-pt
  :config
  (setq-default help-at-pt-display-when-idle '(flymake-diagnostic))
  (help-at-pt-set-timer))

(use-package warnings
  :custom ((warning-minimum-level :error)))

(use-package subword
  :config
  (global-subword-mode 1)
  (delight 'subword-mode nil t))

(use-package paren
  :config
  (show-paren-mode 1)
  (setopt show-paren-when-point-inside-paren t))

(use-package elec-pair
  :config
  (electric-pair-mode 1))

(use-package delsel
  :config
  (delete-selection-mode 1))

(use-package repeat
  :config
  (repeat-mode 1))

(use-package dabbrev
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
  (jacob-defhookf vc-git-log-view-mode-hook
    (jacob-xfk-local-key "q" #'quit-window)))

(use-package vc-dir
  :defer t
  :config
  (jacob-defhookf vc-dir-mode-hook
    (jacob-xfk-local-key "q" #'quit-window)
    (jacob-xfk-local-key "g" #'revert-buffer)
    (jacob-xfk-local-key "i" #'vc-dir-previous-line)
    (jacob-xfk-local-key "k" #'vc-dir-next-line)
    (jacob-xfk-local-key "o" #'vc-dir-next-directory)
    (jacob-xfk-local-key "u" #'vc-dir-previous-directory)
    (jacob-xfk-local-key "s" #'vc-dir-find-file)
    (jacob-xfk-local-key "e" #'vc-dir-mark)
    (jacob-xfk-local-key "r" #'vc-dir-unmark)
    (jacob-xfk-local-key "v" #'vc-next-action)
    (jacob-xfk-local-key "p" #'vc-push)
    (jacob-xfk-local-key ";" #'jacob-git-push-set-upstream)
    (jacob-xfk-local-key "=" #'vc-diff)
    (jacob-xfk-local-key "x" #'vc-dir-hide-up-to-date)))

(use-package vc-annotate
  :defer t
  :config
  (jacob-defhookf vc-annotate-mode-hook
    (jacob-xfk-local-key "q" #'quit-window)
    (jacob-xfk-local-key "g" #'revert-buffer)))

(use-package magit
  :ensure t
  :bind ( :map project-prefix-map
          ("v" . magit)))

(use-package forge
  :ensure t
  :defer t)

(use-package git-gutter-fringe
  :ensure t
  :config
  (global-git-gutter-mode 1)
  (delight 'git-gutter-mode nil t)
  (setopt git-gutter-fr:side 'right-fringe))

(use-package autoinsert
  :config
  (auto-insert-mode t)
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

  (keymap-set jacob-xfk-map "c" `("Code" . ,jacob-code-map))
  :config
  (jacob-defhookf eglot-managed-mode-hook
    (eglot-inlay-hints-mode 0)
    (setq-local eldoc-documentation-strategy 'eldoc-documentation-compose))

  ;; TODO: function that can smartly decide between jumping to
  ;; definition or implementation (`xref-find-definitions' vs
  ;; `eglot-find-implementation')

  ;; WIP
  (defun jacob-go-definition ()
    "If not in an eglot buffer, do regular xref stuff.

Otherwise, go to implementation.  If already at implementation go to
definition."
    (interactive)
    (if (not eglot--managed-mode)
        (call-interactively #'xref-find-definitions)
      (let ((start-buffer (current-buffer)))
        (ignore-errors
          (eglot-find-implementation))
        (when (eq start-buffer (current-buffer))
          ;; TODO: won't work, this just takes us to the current
          ;; method. if language server implemented go to declaration
          ;; something might be possible.
          (call-interactively #'xref-find-definitions)))))

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

  (eglot--code-action eglot-code-action-organize-imports-ts "source.organizeImports.ts")
  (eglot--code-action eglot-code-action-add-missing-imports-ts "source.addMissingImports.ts")

  (setopt eglot-ignored-server-capabilities '(:documentOnTypeFormattingProvider)))

(use-package csharp-mode
  :mode ("//.csx?//'" . csharp-ts-mode)
  :config
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
             ((parent-is "lambda_expression") parent-bol csharp-ts-mode-indent-offset)
             ((parent-is "try_statement") parent-bol 0)
             ((parent-is "catch_clause") parent-bol 0)
             ((parent-is "record_declaration") parent-bol 0)
             ((parent-is "interface_declaration") parent-bol 0)
             ((parent-is "throw_expression") parent-bol csharp-ts-mode-indent-offset)
             ((parent-is "return_statement") parent-bol csharp-ts-mode-indent-offset)
             ((parent-is "record_declaration") parent-bol 0)
             ((parent-is "interface_declaration") parent-bol 0)
             )))

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

  ;; TODO: test the below code!

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

(use-package sharper
  :ensure t
  :after csharp-mode
  :config
  (keymap-set jacob-xfk-map "d" #'sharper-main-transient))

(use-package csproj-mode
  :ensure t)

(use-package font-lock-ext ; dependency of `sln-mode'
  :vc ( :url "https://github.com/sensorflo/font-lock-ext.git"
        :rev :newest))

;; TODO: package `sln-mode' for elpa/melpa?
(use-package sln-mode
  :vc ( :url "https://github.com/sensorflo/sln-mode.git"
        :rev :newest)
  :config
  (add-to-list 'auto-mode-alist '("\\.sln\\'". sln-mode)))

(use-package fsharp-mode
  :defer t
  :config
  (remove-hook 'project-find-functions #'fsharp-mode-project-root)
  (setopt compilation-error-regexp-alist (remq 'fsharp compilation-error-regexp-alist)))

(use-package inf-lisp
  :defer t
  :config
  (setopt inferior-lisp-program "sbcl"))

(use-package ls-lisp
  :demand
  :config
  (setopt ls-lisp-use-insert-directory-program nil
          ls-lisp-dirs-first t))

(use-package dired
  :defer t
  :after ls-lisp
  :config
  (jacob-defhookf dired-mode-hook
    (dired-hide-details-mode 1)
    (jacob-xfk-local-key "s" #'dired-find-file)
    (jacob-xfk-local-key "d" #'dired-do-delete) ; we skip the "flag, delete" process as files are sent to system bin on deletion
    (jacob-xfk-local-key "q" #'quit-window)
    (jacob-xfk-local-key "i" #'dired-previous-line)
    (jacob-xfk-local-key "k" #'dired-next-line)
    (jacob-xfk-local-key "e" #'dired-mark)
    (jacob-xfk-local-key "r" #'dired-unmark)
    (jacob-xfk-local-key "g" #'revert-buffer)
    (jacob-xfk-local-key "x" #'dired-do-rename)
    (jacob-xfk-local-key "c" #'dired-do-copy)
    (jacob-xfk-local-key "u" #'dired-up-directory)
    (jacob-xfk-local-key "j" #'dired-goto-file))
  
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

  (when jacob-is-windows
    (defun jacob-confirm-terminate-batch-job ()
      "Type y and enter to terminate batch job after sending ^C."
      (when (not (null eshell-process-list))
        (insert "y")
        (eshell-send-input)))

    (advice-add 'eshell-interrupt-process :after #'jacob-confirm-terminate-batch-job)))

(use-package pcomplete
  :config
  (defun pcomplete/gco ()
    "Completion for the gco alias on git branches."
    (pcomplete-here* (jacob-git-get-branches)))

  (defun pcomplete/grh ()
    "Completion for the grh alias on git branches."
    (pcomplete-here* (jacob-git-get-branches t))))

(use-package eldoc
  :config
  (global-eldoc-mode 1)
  (delight 'eldoc-mode nil t)
  (setopt eldoc-documentation-strategy 'eldoc-documentation-compose))

(use-package project
  :config
  (setopt project-switch-commands '((project-find-file "Find file")
                                    (jacob-project-search "Find regexp")
                                    (project-find-dir "Find directory")
                                    (magit "Version Control")
                                    (project-eshell "Shell")
                                    (project-compile "Compile"))))

(use-package prodigy
  :ensure t
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

  (keymap-set project-prefix-map "l" #'prodigy)

  (jacob-defhookf prodigy-mode-hook
    (hl-line-mode 0)
    (jacob-xfk-local-key "d" #'prodigy-stop)
    (jacob-xfk-local-key "e" #'prodigy-mark)
    (jacob-xfk-local-key "g" #'jacob-project-search)
    (jacob-xfk-local-key "f" #'project-find-file)
    (jacob-xfk-local-key "i" #'prodigy-prev)
    (jacob-xfk-local-key "k" #'prodigy-next)
    (jacob-xfk-local-key "q" #'quit-window)
    (jacob-xfk-local-key "r" #'prodigy-unmark)
    (jacob-xfk-local-key "s" #'prodigy-restart)
    (jacob-xfk-local-key "v" #'prodigy-display-process))

  (jacob-defhookf prodigy-view-mode-hook
    (compilation-minor-mode 1)
    (jacob-xfk-local-key "q" #'quit-window)
    (jacob-xfk-local-key "g" #'prodigy-restart)))

(use-package hi-lock
  :config
  (delight 'hi-lock-mode nil t))

(use-package highlight-defined
  :ensure t)

(use-package hl-todo
  :config
  (global-hl-todo-mode 1))

(use-package lisp-extra-font-lock
  :ensure t
  :config
  (lisp-extra-font-lock-global-mode 1))

(use-package elisp-mode
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
    (setq-local outline-regexp "^(\\(jacob-\\)*require '\\([a-z-]+\\)")
    (flymake-mode 1)
    (highlight-defined-mode 1)
    (add-hook 'before-save-hook 'jacob-indent-buffer nil "LOCAL")
    (setq-local yas-key-syntaxes '("w_")))

  (add-to-list 'lisp-imenu-generic-expression '(nil "^(\\(jacob-\\)*require '\\([a-z-]+\\)" 2))

  ;; (yas-define-snippets #'emacs-lisp-mode
  ;;                      '(("add-hook" "(add-hook '-hook$0 #')")
  ;;                        ("cond" "(cond ($0t 0))")
  ;;                        ("let" "(let (($0x 1))\n)")
  ;;                        ("save-excursion" "(save-excursion\n$0)")
  ;;                        ("defun" "(defun $0 ()\n)")
  ;;                        ("keymap-set" "(keymap-set '$0 \"\" #')")
  ;;                        ("lambda" "(lambda ($0)\n)")
  ;;                        ("message" "(message $0)")))

  (defun jacob-eval-print-last-sexp ()
    "Run `eval-print-last-sexp', indent the result."
    (interactive)
    (save-excursion
      (eval-print-last-sexp 0))
    (save-excursion
      (forward-line)
      (indent-pp-sexp t)))

  (setopt elisp-flymake-byte-compile-load-path load-path)

  (keymap-set lisp-interaction-mode-map "C-j" #'jacob-eval-print-last-sexp)
  (keymap-set lisp-interaction-mode-map "(" #'insert-parentheses)
  (keymap-set lisp-interaction-mode-map ")" #'move-past-close-and-reindent)

  (keymap-set emacs-lisp-mode-map "(" #'insert-parentheses)
  (keymap-set emacs-lisp-mode-map ")" #'move-past-close-and-reindent))

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
  (jacob-defhookf geiser-mode-hook
    (jacob-xfk-local-key "SPC , m" #'geiser-eval-last-sexp)
    (jacob-xfk-local-key "SPC , d" #'geiser-eval-definition)))

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

  (keymap-set jacob-xfk-map "a" `("Agenda" . ,jacob-org-agenda-map))  
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
    (hl-line-mode 1)
    (jacob-xfk-local-key "q" #'quit-window)
    (jacob-xfk-local-key "g" #'org-agenda-redo-all)))

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
  :config
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
  :config
  (server-start))

(use-package smerge-mode
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
  (jacob-defhookf calendar-mode-hook
    (jacob-xfk-local-key "q" 'quit-window)
    (jacob-xfk-local-key "i" 'calendar-backward-week)
    (jacob-xfk-local-key "k" 'calendar-forward-week)
    (jacob-xfk-local-key "j" 'calendar-backward-day)
    (jacob-xfk-local-key "l" 'calendar-forward-day)
    (jacob-xfk-local-key "u" 'calendar-backward-month)
    (jacob-xfk-local-key "o" 'calendar-forward-month)
    (jacob-xfk-local-key "d" 'diary-view-entries)
    (jacob-xfk-local-key "s" 'diary-insert-entry)
    (jacob-xfk-local-key "m" 'diary-mark-entries)
    (jacob-xfk-local-key "." 'calendar-goto-today)
    (jacob-xfk-local-key "t" 'calendar-set-mark))

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
  :init
  (with-eval-after-load "xah-fly-keys"
    (keymap-set xah-fly-command-map "1" #'winner-undo)
    (keymap-set xah-fly-command-map "2" #'winner-redo))
  :config
  (winner-mode 1))

(use-package compile
  :defer t
  :init
  (keymap-global-set "<f5>" 'recompile)
  :config
  (jacob-defhookf compilation-mode-hook
    (jacob-xfk-local-key "g" #'recompile)
    (jacob-xfk-local-key "q" #'quit-window))

  (jacob-defhookf compilation-filter-hook
    (ansi-color-compilation-filter))

  (setopt compilation-always-kill t
          compilation-scroll-output t))

(use-package sql
  :config
  
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

  (jacob-defhookf sql-interactive-mode-hook
    (when (eq sql-product 'postgres)
      (setq sql-prompt-regexp "^[-[:alnum:]_]*[-=]\\*?[#>] ")
      (setq sql-prompt-cont-regexp "^\\(?:\\sw\\|\\s_\\)*[-(]\\*?[#>] "))
    (jacob-xfk-local-key "SPC , d" #'sql-send-paragraph))

  (defun jacob-sqli-end-of-buffer ()
    "Move point to end of sqli buffer before sending paragraph.

Intended as before advice for `sql-send-paragraph'."
    (with-current-buffer sql-buffer
      (goto-char (point-max))))

  (advice-add #'sql-send-paragraph :before #'jacob-sqli-end-of-buffer)

  (keymap-set jacob-xfk-map "s" #'jacob-sql-connect))

(use-package doc-view
  :defer t
  :config
  (jacob-defhookf doc-view-mode-hook
    (auto-revert-mode 1)
    (jacob-xfk-local-key "l" 'doc-view-next-page)
    (jacob-xfk-local-key "j" 'doc-view-previous-page)))

(use-package treesit
  :config
  (setopt treesit-font-lock-level 4))

(use-package treesit-auto
  :ensure t
  :config
  (global-treesit-auto-mode 1)
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

(use-package gnus
  :commands gnus
  :init
  (keymap-set jacob-xfk-map "g" #'gnus)
  :config
  (jacob-defhookf gnus-started-hook
    (gnus-demon-add-handler 'gnus-demon-scan-news 2 t))

  (setopt gnus-use-full-window t
          gnus-always-read-dribble-file t))

(use-package gnus-group
  :after gnus
  :config
  (jacob-defhookf gnus-group-mode-hook
    (jacob-xfk-local-key "q" #'gnus-group-exit)
    (jacob-xfk-local-key "i" #'gnus-group-prev-group)
    (jacob-xfk-local-key "k" #'gnus-group-next-group)
    (jacob-xfk-local-key "g" #'gnus-group-get-new-news)))

(use-package gnus-notifications
  :after gnus
  :config
  (add-hook 'gnus-after-getting-new-news-hook 'gnus-notifications))

(use-package gnus-sum
  :after gnus
  :config
  (jacob-defhookf gnus-summary-mode-hook
    (jacob-xfk-local-key "q" #'gnus-summary-exit)
    (jacob-xfk-local-key "i" #'gnus-summary-prev-article)
    (jacob-xfk-local-key "k" #'gnus-summary-next-article)
    (jacob-xfk-local-key "j" #'gnus-summary-prev-page)
    (jacob-xfk-local-key "l" #'gnus-summary-next-page)))

(use-package gnus-topic
  :after gnus
  :config
  (add-hook 'gnus-group-mode-hook 'gnus-topic-mode)
  
  (defun jacob-gnus-topic-mode-hook-function ()
    "Hook function to be used with `gnus-topic-mode-hook'."
    (jacob-xfk-local-key "s" #'gnus-topic-select-group)))

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
                               (?r . jacob-avy-eglot-rename)))

  (key-chord-define-global "fj" #'avy-goto-char-timer)

  (keymap-global-set "M-j" #'avy-goto-char-timer)
  (keymap-set isearch-mode-map "M-j" #'avy-isearch))

(use-package apheleia
  :ensure t
  :delight
  :config
  (apheleia-global-mode 1)
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
  :config
  (add-hook 'prog-mode-hook #'rainbow-mode)
  (delight 'rainbow-mode))

(use-package eglot-booster
  :after eglot
  :when (executable-find "emacs-lsp-booster")
  :vc ( :url "https://github.com/jdtsmith/eglot-booster"
        :rev :newest)
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
  :config
  (setopt aw-keys '(?a ?s ?d ?f ?q ?w ?e ?r)
          aw-minibuffer-flag t))

(use-package tex
  :ensure auctex
  :commands (TeX-PDF-mode)
  :mode ("\\.tex\\'" . latex-mode)
  :config
  (jacob-defhookf LaTeX-mode-hook
    (visual-fill-column-mode 1)
    (toggle-word-wrap 1)
    (TeX-PDF-mode 1))
  :custom ((TeX-auto-save t)
           (TeX-parse-self t)
           (japanese-TeX-error-messages nil)))

(use-package visual-fill-column
  :ensure t)

(use-package markdown-mode
  :ensure t)

(use-package feature-mode
  :ensure t)

(use-package vertico
  :ensure t
  :config
  (vertico-mode 1))

(use-package vertico-mouse
  :config
  (vertico-mouse-mode 1))

(use-package orderless
  :ensure t
  :defer t
  :preface
  (defun jacob-load-orderless ()
    "Load the `orderless' library."
    (require 'orderless))
  :hook (jacob-first-minibuffer-activation-hook . jacob-load-orderless)
  :config
  (setopt completion-styles '(orderless initials)))

(use-package marginalia
  :ensure t
  :defer t
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
    (keymap-set xah-fly-command-map "\\" #'embark-act))
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
  :hook (org-mode-hook . #'verb-mode)
  :config
  (jacob-defhookf verb-response-body-mode-hook
    (jacob-xfk-local-key "q" #'quit-window))

  (defun jacob-verb-id (response-id)
    "Get the id property from the stored verb response pertaining to RESPONSE-ID."
    (verb-json-get (oref (verb-stored-response response-id) body) "id")))

(use-package sly
  :ensure t
  :mode ("\\.lisp\\'")
  :config
  (sly-setup)

  (sly-symbol-completion-mode 0)

  (jacob-defhookf sly-mode-hook
    (jacob-xfk-local-key "SPC , m" #'sly-eval-last-expression)
    (jacob-xfk-local-key "SPC , d" #'sly-compile-defun)
    (jacob-xfk-local-key "SPC , e" #'sly-eval-buffer)
    (jacob-xfk-local-key "SPC w k" #'sly-edit-definition))

  (setopt sly-auto-start 'always)

  (jacob-defhookf sly-db-hook
    (jacob-xfk-local-key "q" #'sly-db-quit)))

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
  :ensure t)

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
  (jacob-defhookf gdscript-ts-mode-hook
    (setopt indent-tabs-mode t))

  (push '((gdscript-mode gdscript-ts-mode) "localhost" 6008) eglot-server-programs)

  (add-to-list 'treesit-auto-recipe-list (make-treesit-auto-recipe
                                          :lang 'gdscript
                                          :ts-mode 'gdscript-ts-mode
                                          :remap 'gdscript-mode
                                          :url "https://github.com/PrestonKnopp/tree-sitter-gdscript.git"
                                          :ext "\\.gd\\'"))

  (add-to-list 'treesit-auto-langs 'gdscript))

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

(use-package pulseaudio-control
  :ensure t
  :defer t
  :when jacob-is-linux)

;; use `pulseaudio-control-select-sink-by-name' to set the "sink" (the
;; output device)

(use-package bluetooth
  :ensure t
  :defer t
  :when jacob-is-linux)

;; use `bluetooth-list-devices' to display the bluetooth buffer

(use-package grep
  :when jacob-is-windows
  :defer t
  :config
  (setopt find-program "C:/Program Files (x86)/GnuWin32/bin/find.exe"))

;; (jacob-require slack)

;; (defun jacob-slack-modeline-formatter (alist)
;;   "Hide the slack modeline if there are no notifications.

;; Element in ALIST is ((team-name . ((thread . (has-unreads
;; . mention-count)) (channel . (has-unreads . mention-count)))))"
;;   (if (seq-find (lambda (team)
;;                   (seq-find (lambda (room-type)
;;                               (car (cdr room-type)))
;;                             (cdr team)))
;;                 alist)
;;       (slack-default-modeline-formatter alist)
;;     ""))

;; (defun jacob-slack-show-unread ()
;;   "Open an unread slack message."
;;   (interactive)
;;   (let* ((team (slack-team-select))
;;          (rooms (seq-filter #'(lambda (room)
;;                                 (slack-room-has-unread-p room team))
;;                             (append (slack-team-ims team)
;;                                     (slack-team-groups team)
;;                                     (slack-team-channels team)))))
;;     (if (null rooms)
;;         (message "no unread slack messages")
;;       (slack-room-display (seq-first rooms) team))))

;; (setopt slack-enable-global-mode-string t
;;         slack-buffer-emojify t
;;         slack-prefer-current-team t
;;         slack-thread-also-send-to-room nil
;;         slack-modeline-formatter #'jacob-slack-modeline-formatter)

;; (setopt alert-default-style 'notifications)

;; (setopt lui-fill-type nil
;;         lui-time-stamp-position 0
;;         lui-time-stamp-format "%a %b %e %H:%M")

;; (defun jacob-slack-hook-function ()
;;   "Function to be run in slack mode hooks."
;;   (toggle-word-wrap 1))

;; (add-hook 'slack-message-buffer-mode-hook 'jacob-slack-hook-function)
;; (add-hook 'slack-thread-message-buffer-mode-hook 'jacob-slack-hook-function)

;; (add-to-list 'display-buffer-alist '((or (derived-mode . slack-mode)
;;                                          (derived-mode . lui-mode))
;;                                      (display-buffer-in-side-window)
;;                                      (side . right)))

;; (defun jacob-consult-slack-filter ()
;;   "Filter for slack buffers."
;;   (consult--buffer-query :sort 'visibility
;;                          :as #'buffer-name
;;                          :include "^*slack"))

;; (defvar jacob-consult-slack-source
;;   `( :name     "Slack"
;;      :narrow   ?s
;;      :category buffer
;;      :face     consult-buffer
;;      :history  buffer-name-history
;;      :items    ,#'jacob-consult-slack-filter
;;      :action   ,#'switch-to-buffer))

;; (add-to-list 'consult-buffer-sources jacob-consult-slack-source "APPEND")


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

(defun jacob-toggle-modeline ()
  "Toggle visibility of modeline."
  (interactive)
  (if mode-line-format
      (setq mode-line-format nil)
    (set 'mode-line-format (eval
                            (car
                             (get 'mode-line-format
                                  'standard-value))))))

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

(defun jacob-git-push-set-upstream ()
  "Push current git branch to new upstream branch."
  (interactive)
  (shell-command "git push --set-upstream origin HEAD"))

(defun jacob-github-push-set-upstream ()
  "Push current git branch to new upstream branch.
Try go to the create a pull request page if possible."
  (interactive)
  (with-temp-buffer
    (shell-command "git push --set-upstream origin HEAD" t)
    (when (search-forward "http" nil "NOERROR")
      (browse-url-at-point))))

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

(defun jacob-gitlab-link-at-point ()
  "Create a gitlab link for the line of code at point."
  (interactive)
  (let* ((project (project-current))
         (repository (project-name project))
         (branch "development")
         (filepath (file-relative-name buffer-file-name (project-root project)))
         (line-number (line-number-at-pos))
         (link (format "https://gitlab.com/tappit-leeds-devs/%s/-/blob/%s/%s#L%d"
                       repository
                       branch
                       filepath
                       line-number)))
    (kill-new link)
    (message "%s" link)))

(provide 'init)

;;; init.el ends here
