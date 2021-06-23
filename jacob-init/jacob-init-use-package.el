;; (unless (package-installed-p 'use-package)
;;   (package-refresh-contents)
;;   (package-install 'use-package))

;; (eval-and-compile
;;   (setq use-package-always-ensure nil)
;;   (setq use-package-always-defer nil)
;;   (setq use-package-always-demand nil)
;;   (setq use-package-enable-imenu-support t)
;;   (setq use-package-hook-name-suffix nil) ; use real name of hook instead of shorter version.
;;   )

;; (eval-when-compile
;;   (require 'use-package))

;; (use-package use-package-chords
;;   :ensure t)
