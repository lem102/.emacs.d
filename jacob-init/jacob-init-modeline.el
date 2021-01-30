(use-package emacs
  :config
  (setq-default mode-line-format (list "%*" ; saved, readonly 
                                       "%m: " ; major mode
                                       "%b " ; buffer name
                                       "(%l,%c)" ; position of point
                                       )))
