(use-package emacs
  :config
  (tool-bar-mode 0)
  (menu-bar-mode 0)
  (scroll-bar-mode 0)
  (setq use-file-dialog nil)
  (setq use-dialog-box t)
  (setq inhibit-startup-message nil)
  ;; TODO: Below needs to be moved to another file.
  :bind
  (("C-z" . nil)
   ("C-c C-z" . nil)
   ("C-h h" . nil)))
