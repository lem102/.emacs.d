;;; jacob-sharper.el --- Configuration for sharper.el  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;; (require 'jacob-xah-fly-keys)

;; (defun jacob-sharper-config ()
;;   "Configure `sharper'."
;;   (keymap-set jacob-xfk-map "d" #'sharper-main-transient))

(use-package sharper
  :ensure t
  :after csharp-mode
  ;; :config
  ;; (jacob-sharper-config)
  )

(provide 'jacob-sharper)

;;; jacob-sharper.el ends here
