(use-package ace-window
  :config
  (setq aw-keys '(?a ?s ?d ?f ?j ?k ?l ?\;))
  (setq aw-background t)
  :bind
  (:map xah-fly-command-map
        ("," . ace-window)))
