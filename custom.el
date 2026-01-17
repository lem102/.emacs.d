;;; -*- lexical-binding: t -*-
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-insert-query nil)
 '(bookmark-fringe-mark nil)
 '(bookmark-watch-bookmark-file 'silent)
 '(completion-styles '(orderless basic initials))
 '(custom-enabled-themes '(ef-bio))
 '(custom-safe-themes
   '("76ddb2e196c6ba8f380c23d169cf2c8f561fd2013ad54b987c516d3cabc00216"
     "d2c76098def8b2b10b45d2092c86ca9c8b95d58fabbc8850d28899181d8f6581"
     "1a721551e5867225da30177ecda385083732873269aa9f1e7188564c01210e1f"
     "2896501d2809d956f0b4fa5442f416cb3e62c82da0ef7ccbef538c67872d1967"
     "ae20535e46a88faea5d65775ca5510c7385cbf334dfa7dde93c0cd22ed663ba0"
     "b41d0a9413fb0034cea34eb8c9f89f6e243bdd76bccecf8292eb1fefa42eaf0a"
     default))
 '(dabbrev-case-fold-search nil)
 '(dabbrev-case-replace nil)
 '(eshell-scroll-to-bottom-on-output 'this)
 '(imenu-use-popup-menu 'on-mouse)
 '(message-send-mail-function 'smtpmail-send-it)
 '(package-archives
   '(("gnu" . "https://elpa.gnu.org/packages/")
     ("nongnu" . "https://elpa.nongnu.org/nongnu/")
     ("melpa" . "https://melpa.org/packages/")))
 '(package-install-upgrade-built-in t)
 '(package-selected-packages
   '(ace-window aider apheleia applescript-mode auctex avy blackout cape
                color-theme-sanityinc-tomorrow consult csproj-mode
                dape denote dired-rsync dumb-jump eat ef-themes eglot
                eglot-booster eldoc embark embark-consult
                exec-path-from-shell expreg feature-mode flymake
                font-lock-ext forge gdscript-mode geiser geiser-guile
                gptel helpful highlight-defined hl-todo just-mode
                lisp-extra-font-lock magit marginalia markdown-mode
                mct mermaid-mode nerd-icons-dired nerd-icons-modeline
                no-littering ob-mermaid on orderless org-edna
                pdf-tools prodigy puni rainbow-mode ryo-modal sbt-mode
                scala-repl scala-ts-mode sharper sln-mode sly
                sly-macrostep sly-overlay sly-quicklisp sql-indent
                treesit-auto verb vertico visual-replace
                visual-replace-regexp web-mode wgrep winnow yasnippet))
 '(package-vc-selected-packages
   '((eglot-booster :url "https://github.com/jdtsmith/eglot-booster")
     (sln-mode :url "https://github.com/sensorflo/sln-mode.git")
     (font-lock-ext :url
                    "https://github.com/sensorflo/font-lock-ext.git")
     (nerd-icons-mode-line :url
                           "https://github.com/grolongo/nerd-icons-mode-line.git")))
 '(safe-local-variable-values
   '((Log . clx.log) (Package . Xlib) (Lowercase . Yes) (Base . 10)
     (Package . XLIB) (Syntax . Common-lisp) (flymake-mode)))
 '(use-package-compute-statistics nil)
 '(use-package-enable-imenu-support t)
 '(use-package-hook-name-suffix nil)
 '(use-package-verbose t)
 '(vc-git-show-stash 0 nil nil "Do not show any stashes in vc-dir buffers.")
 '(vc-ignore-dir-regexp
   (format "\\(%s\\)\\|\\(%s\\)" locate-dominating-stop-dir-regexp
           tramp-file-name-regexp) nil nil "In addition to the usual files, also disable vc functionality in tramp files."))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
