(setq kill-buffer-query-functions (delq 'process-kill-buffer-query-function kill-buffer-query-functions))
(setq read-process-output-max (* 1024 1024))
(setq ring-bell-function 'ignore)
(setq auto-window-vscroll nil)
(setq scroll-conservatively 100)
(setq create-lockfiles nil)
(setq history-length 1000)
(setq history-delete-duplicates t)
(setq-default truncate-lines nil)
(defalias 'yes-or-no-p 'y-or-n-p)

(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

(setq confirm-kill-processes nil)
(setq backup-by-copying t)
(setq make-backup-files nil)
(setq auto-save-default nil)

(add-hook 'after-init-hook (lambda () (recentf-mode 1)))

(setq dabbrev-case-fold-search nil)
(setq dabbrev-case-replace nil)

(setq custom-file (make-temp-file "emacs-custom-"))

(setq disabled-command-function nil)

(setq line-move-visual t)

(setq ls-lisp-use-insert-directory-program nil)
(setq ls-lisp-dirs-first t)

(global-subword-mode 1)

(delete-selection-mode 1)

(setq show-paren-style 'parenthesis)
(setq show-paren-when-point-inside-paren t)
(setq show-paren-when-point-in-periphery nil)
(show-paren-mode 1)

(setq electric-pair-preserve-balance t)
(setq electric-pair-delete-adjacent-pairs t)
(setq electric-pair-open-newline-between-pairs t)
(electric-pair-mode 1)

(setq ibuffer-expert t)

(setq savehist-file "~/.emacs.d/savehist")
(setq savehist-save-minibuffer-history t)
(savehist-mode 1)

(setq save-place-file "~/.emacs.d/saveplace")
(setq save-place-forget-unreadable-files t)
(save-place-mode 1)

(setq use-file-dialog nil)
(setq use-dialog-box t)
(setq inhibit-startup-message nil)
