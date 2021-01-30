;;; -*- lexical-binding: t -*-

;; garbage collection stolen from doom emacs

(setq gc-cons-threshold most-positive-fixnum)
(setq gc-cons-percentage 0.6)

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold 16777216) ; 16mb
            (setq gc-cons-percentage 0.1)))

;; tweak garbage collection when using the minibuffer

(defun doom-defer-garbage-collection-h ()
  (setq gc-cons-threshold most-positive-fixnum))

(defun doom-restore-garbage-collection-h ()
  (run-at-time
   1 nil (lambda () (setq gc-cons-threshold 16777216))))

(add-hook 'minibuffer-setup-hook #'doom-defer-garbage-collection-h)
(add-hook 'minibuffer-exit-hook #'doom-restore-garbage-collection-h)
