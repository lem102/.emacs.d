;; gui setup

(unless (display-graphic-p)
  (scroll-bar-mode 0)
  (tool-bar-mode 0))
(menu-bar-mode 0)

;; garbage collection

(setq gc-cons-threshold most-positive-fixnum)
(setq gc-cons-percentage 0.6)

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold 16777216) ; 16mb
            (setq gc-cons-percentage 0.1)))

(setq package-enable-at-startup nil)
