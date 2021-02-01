(setq kill-buffer-query-functions (delq 'process-kill-buffer-query-function kill-buffer-query-functions))
(setq read-process-output-max (* 1024 1024))
(setq ring-bell-function 'ignore)
(setq auto-window-vscroll nil)
(setq scroll-conservatively 100)
(setq create-lockfiles nil)
(setq history-length 1000)
(setq history-delete-duplicates t)

(require 'files)
(with-eval-after-load 'files
  (setq confirm-kill-processes nil)
  (setq backup-by-copying t)
  (setq make-backup-files nil)
  (setq auto-save-default nil))

(require 'recentf)
(with-eval-after-load 'recentf
  (add-hook 'after-init-hook #'recentf-mode))

(require 'dabbrev)
(with-eval-after-load 'dabbrev
  (setq dabbrev-case-fold-search nil)
  (setq dabbrev-case-replace nil))

(require 'cus-edit)
(with-eval-after-load 'cus-edit
  (setq custom-file (make-temp-file "emacs-custom-")))

(require 'novice)
(with-eval-after-load 'novice
  (setq disabled-command-function nil))

(require 'simple)
(with-eval-after-load 'simple
  (setq line-move-visual t))

;; TODO: figure out how to group these when on windows
(setq ls-lisp-use-insert-directory-program nil)
(setq ls-lisp-dirs-first t)

(require 'subword)
(with-eval-after-load 'subword
  (add-hook 'after-init-hook #'global-subword-mode))

(require 'delsel)
(with-eval-after-load 'delsel
  (add-hook 'after-init-hook #'delete-selection-mode))

(require 'paren)
(with-eval-after-load 'paren
  (setq show-paren-style 'parenthesis)
  (setq show-paren-when-point-in-periphery nil)
  (setq show-paren-when-point-inside-paren nil)
  (add-hook 'after-init-hook #'show-paren-mode))

(defalias 'yes-or-no-p 'y-or-n-p)

(require 'ibuffer)
(with-eval-after-load 'ibuffer
  (setq ibuffer-expert t))

(require 'savehist)
(with-eval-after-load 'savehist
  (setq savehist-file "~/.emacs.d/savehist")
  (setq savehist-save-minibuffer-history t))

(require 'saveplace)
(with-eval-after-load 'saveplace
  (setq save-place-file "~/.emacs.d/saveplace")
  (setq save-place-forget-unreadable-files t)
  (add-hook 'after-init-hook #'save-place-mode))
