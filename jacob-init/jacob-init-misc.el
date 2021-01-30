;; ** don't ask for conformation when killing buffers with an attached process
(use-package emacs
  :config
  (setq kill-buffer-query-functions (delq 'process-kill-buffer-query-function kill-buffer-query-functions))
  ;; this is for asking on exit
  (setq confirm-kill-processes nil))
;; ** recentf
(use-package recentf
  :commands recentf-open-files
  :config
  (recentf-mode 1))
;; ** dabbrev
(use-package dabbrev
  :config
  (setq dabbrev-case-fold-search nil)
  (setq dabbrev-case-replace nil))
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
(setq redisplay-dont-pause t) ; obsolete
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
