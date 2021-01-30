(use-package emacs
  :config
  ;; use spaces to indent
  (setq-default indent-tabs-mode nil)
  ;; set default tab char's display width to 4 spaces
  (setq-default tab-width 4)
  ;; make tab key call indent command or insert tab character, depending on cursor position
  (setq-default tab-always-indent 'complete))

