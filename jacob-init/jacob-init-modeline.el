(column-number-mode 1)

(setq-default mode-line-format (list "%*" ; saved, readonly 
                                     "%m: " ; major mode
                                     "%b " ; buffer name
                                     "(%l,%c)" ; position of point
                                     ))
