;;; -*- lexical-binding: t -*-
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-insert-query nil)
 '(auto-save-default nil)
 '(auto-save-visited-interval 2 nil nil "Save file after two seconds.")
 '(backup-by-copying t)
 '(bookmark-fringe-mark nil)
 '(bookmark-watch-bookmark-file 'silent)
 '(completion-ignore-case t t)
 '(completion-styles '(orderless basic initials))
 '(confirm-kill-processes nil)
 '(create-lockfiles nil)
 '(custom-enabled-themes '(modus-vivendi-tinted))
 '(custom-safe-themes
   '("21c4c4b7d3ab161aaa28b15ca846854d395c33cfb7c6863ab601adfe10d70ce0"
     "76ddb2e196c6ba8f380c23d169cf2c8f561fd2013ad54b987c516d3cabc00216"
     "d2c76098def8b2b10b45d2092c86ca9c8b95d58fabbc8850d28899181d8f6581"
     "1a721551e5867225da30177ecda385083732873269aa9f1e7188564c01210e1f"
     "2896501d2809d956f0b4fa5442f416cb3e62c82da0ef7ccbef538c67872d1967"
     "ae20535e46a88faea5d65775ca5510c7385cbf334dfa7dde93c0cd22ed663ba0"
     "b41d0a9413fb0034cea34eb8c9f89f6e243bdd76bccecf8292eb1fefa42eaf0a"
     default))
 '(dabbrev-case-fold-search nil)
 '(dabbrev-case-replace nil)
 '(delete-by-moving-to-trash t)
 '(electric-indent-mode nil nil nil "Enabled by default.")
 '(enable-recursive-minibuffers t)
 '(eshell-scroll-to-bottom-on-output 'this)
 '(frame-resize-pixelwise t)
 '(history-delete-duplicates t)
 '(history-length 1000)
 '(imenu-use-popup-menu 'on-mouse)
 '(indent-tabs-mode nil nil nil "Use spaces to indent.")
 '(kill-buffer-query-functions
   (delq 'process-kill-buffer-query-function kill-buffer-query-functions) t)
 '(kill-do-not-save-duplicates t)
 '(make-backup-files nil)
 '(message-send-mail-function 'smtpmail-send-it)
 '(mode-line-percent-position nil)
 '(modus-themes-to-toggle '(modus-operandi-tinted modus-vivendi-tinted))
 '(org-default-notes-file "~/Documents/notes.org")
 '(package-archives
   '(("gnu" . "https://elpa.gnu.org/packages/")
     ("nongnu" . "https://elpa.nongnu.org/nongnu/")
     ("melpa" . "https://melpa.org/packages/")))
 '(package-install-upgrade-built-in t)
 '(package-selected-packages
   '(ace-window aider apheleia applescript-mode auctex avy blackout cape
                color-theme-sanityinc-tomorrow consult csproj-mode
                dape denote dired-rsync dumb-jump eat editorconfig
                ef-themes eglot eglot-booster eldoc embark
                embark-consult erc exec-path-from-shell expreg faceup
                feature-mode flymake font-lock-ext forge gdscript-mode
                geiser geiser-guile gptel helpful highlight-defined
                hl-todo idlwave jsonrpc just-mode lisp-extra-font-lock
                magit marginalia markdown-mode mct mermaid-mode
                nerd-icons-dired nerd-icons-mode-line
                nerd-icons-modeline no-littering ob-mermaid on
                orderless org org-edna pdf-tools peg prodigy project
                puni python rainbow-mode ryo-modal sbt-mode scala-repl
                scala-ts-mode sharper sln-mode sly sly-macrostep
                sly-overlay sly-quicklisp sql-indent track-changes
                tramp treesit-auto verb verilog-mode vertico
                visual-replace visual-replace-regexp web-mode wgrep
                which-key window-tool-bar winnow yasnippet))
 '(package-vc-selected-packages
   '((eglot-booster :url "https://github.com/jdtsmith/eglot-booster")
     (sln-mode :url "https://github.com/sensorflo/sln-mode.git")
     (font-lock-ext :url
                    "https://github.com/sensorflo/font-lock-ext.git")
     (nerd-icons-mode-line :url
                           "https://github.com/grolongo/nerd-icons-mode-line.git")))
 '(read-extended-command-predicate 'command-completion-default-include-p)
 '(read-process-output-max 1048576 t)
 '(recentf-max-saved-items nil)
 '(remote-file-name-inhibit-auto-save-visited t)
 '(ring-bell-function 'ignore)
 '(safe-local-variable-values
   '((Log . clx.log) (Package . Xlib) (Lowercase . Yes) (Base . 10)
     (Package . XLIB) (Syntax . Common-lisp) (flymake-mode)))
 '(save-interprogram-paste-before-kill t)
 '(scroll-conservatively 0)
 '(split-height-threshold nil)
 '(switch-to-buffer-obey-display-actions t)
 '(tab-always-indent 'complete)
 '(tab-width 4 nil nil "set default tab char's display width to 4 spaces")
 '(tramp-copy-size-limit 1048576)
 '(tramp-use-scp-direct-remote-copying t)
 '(truncate-partial-width-windows nil)
 '(use-dialog-box t)
 '(use-package-compute-statistics nil)
 '(use-package-enable-imenu-support t)
 '(use-package-hook-name-suffix nil)
 '(use-package-verbose t)
 '(use-short-answers t)
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

