(use-package emacs
  :defer 2
  :config
  (setq kill-buffer-query-functions (delq 'process-kill-buffer-query-function kill-buffer-query-functions))
  (setq read-process-output-max (* 1024 1024))
  (setq ring-bell-function 'ignore)
  (setq auto-window-vscroll nil)
  (setq scroll-conservatively 100)
  (setq create-lockfiles nil)
  (setq history-length 1000)
  (setq history-delete-duplicates t)
  (setq-default truncate-lines nil)
  (defalias 'yes-or-no-p 'y-or-n-p))

(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

(use-package files
  :defer 2
  :config
  (setq confirm-kill-processes nil)
  (setq backup-by-copying t)
  (setq make-backup-files nil)
  (setq auto-save-default nil))

(add-hook 'after-init-hook (lambda () (recentf-mode 1)))

(use-package dabbrev
  :defer 2
  :config
  (setq dabbrev-case-fold-search nil)
  (setq dabbrev-case-replace nil))

(use-package cus-edit
  :defer 2
  :config
  (setq custom-file (make-temp-file "emacs-custom-")))

(use-package novice
  :defer 2
  :config
  (setq disabled-command-function nil))

(use-package simple
  :defer 2
  :config
  (setq line-move-visual t))

(setq ls-lisp-use-insert-directory-program nil)
(setq ls-lisp-dirs-first t)

(use-package subword
  :defer 2
  :config
  (global-subword-mode 1))

(use-package delsel
  :defer 2
  :config
  (delete-selection-mode 1))

(use-package paren
  :defer 2
  :config
  (setq show-paren-style 'parenthesis)
  (setq show-paren-when-point-in-periphery nil)
  (setq show-paren-when-point-inside-paren nil)
  (show-paren-mode 1))

(use-package ibuffer
  :config
  (setq ibuffer-expert t))

(use-package savehist
  :defer 2
  :config
  (setq savehist-file "~/.emacs.d/savehist")
  (setq savehist-save-minibuffer-history t)
  (savehist-mode 1))

(use-package saveplace
  :defer 2
  :config
  (setq save-place-file "~/.emacs.d/saveplace")
  (setq save-place-forget-unreadable-files t)
  (save-place-mode 1))

(use-package kmacro
  :after xah-fly-keys
  :bind
  (:map xah-fly-r-keymap
        ("c" . kmacro-set-counter)))
