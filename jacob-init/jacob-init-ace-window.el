(autoload 'ace-window "ace-window")
(eval-after-load "ace-window"
  (progn
    (setq aw-keys '(?a ?s ?d ?f ?j ?k ?l ?\;))
    (setq aw-background t)))
