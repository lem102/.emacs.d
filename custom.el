;;; -*- lexical-binding: t -*-
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-insert-query nil)
 '(auto-save-default nil)
 '(auto-save-visited-interval 2 nil nil "Save file after two seconds.")
 '(avy-dispatch-alist
   '((120 . avy-action-kill-stay) (88 . jacob-avy-kill-line)
     (116 . avy-action-teleport) (103 . avy-action-mark)
     (99 . avy-action-copy) (67 . jacob-avy-copy-line)
     (118 . avy-action-yank) (86 . jacob-avy-yank-line)
     (105 . avy-action-ispell) (122 . avy-action-zap-to-char)
     (92 . jacob-avy-embark)))
 '(avy-keys '(97 115 100 102 104 106 108 59))
 '(backup-by-copying t)
 '(bookmark-fringe-mark nil)
 '(bookmark-watch-bookmark-file 'silent)
 '(completion-ignore-case t t)
 '(completion-styles '(orderless basic initials))
 '(confirm-kill-processes nil)
 '(consult-git-log-grep-open-function 'magit-show-commit)
 '(consult-project-function 'consult-project-extra-project-fn)
 '(create-lockfiles nil)
 '(custom-enabled-themes '(modus-vivendi-tinted))
 '(custom-safe-themes
   '("1b7e575c6681e66d8d83634c2c160b40af12f3756360a4dd81b8032f4495cb5e"
     "0a8cf72fd94bfb67dd72dc085538b39ea47aeae8bfc2b8545c0d3c99c339c204"
     "bb89e9c403adb33be978454700eecc1868a40ef2f8da0dccfff3a1d274532641"
     "647705ec8322b0464de13e617bd6a074f49505e6e289af189efb820ac88a777d"
     "5504a3ba1ce2196a3fee893cbc6c123275719ef60a8a5cad245a8fda43d32e2a"
     "9a20d47259ea36195dd76eca409866882e8b47fd3b0d377566c107dae0ff60f9"
     "c8b84cff70a0e7a26e1a67a58a5066ff3b28bb2fd0ded47e71cc94da52ddd03a"
     "7c461d294a169dbb83ba621be52fd83f4cf59cc48a88e9407c0d6a741cb4d950"
     "21c4c4b7d3ab161aaa28b15ca846854d395c33cfb7c6863ab601adfe10d70ce0"
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
 '(grep-use-headings t)
 '(help-enable-variable-value-editing t)
 '(help-window-select t)
 '(hippie-expand-try-functions-list
   '(try-complete-file-name-partially try-complete-file-name
                                      try-expand-all-abbrevs
                                      try-expand-line
                                      try-expand-dabbrev
                                      try-expand-dabbrev-all-buffers
                                      try-expand-dabbrev-from-kill
                                      try-complete-lisp-symbol-partially
                                      try-complete-lisp-symbol))
 '(history-delete-duplicates t)
 '(history-length 1000)
 '(imenu-use-popup-menu 'on-mouse)
 '(indent-tabs-mode nil nil nil "Use spaces to indent.")
 '(isearch-lazy-count t)
 '(kill-buffer-query-functions
   (remq 'process-kill-buffer-query-function kill-buffer-query-functions) t)
 '(kill-do-not-save-duplicates t)
 '(magit-buffer-name-format "*%x%M%v: %t%x*")
 '(magit-section-initial-visibility-alist '((untracked . show) (stashes . show)))
 '(make-backup-files nil)
 '(mcp-hub-servers
   '(("elisp-dev" :command "~/.emacs.d/emacs-mcp-stdio.sh" :args
      ("--init-function=elisp-dev-mcp-enable"
       "--stop-function=elisp-dev-mcp-disable"
       "--server-id=elisp-dev-mcp"))))
 '(message-send-mail-function 'smtpmail-send-it)
 '(mode-line-percent-position nil)
 '(modus-themes-to-toggle '(modus-operandi-tinted modus-vivendi-tinted))
 '(mouse-1-double-click-prefer-symbols t)
 '(mouse-drag-copy-region 'non-empty)
 '(org-default-notes-file "~/Documents/notes.org")
 '(package-archives
   '(("gnu" . "https://elpa.gnu.org/packages/")
     ("nongnu" . "https://elpa.nongnu.org/nongnu/")
     ("melpa" . "https://melpa.org/packages/")))
 '(package-install-upgrade-built-in t)
 '(package-selected-packages
   '(ace-window apheleia applescript-mode auctex avy blackout
                caddyfile-mode cape color-theme-sanityinc-tomorrow
                consult consult-git-log-grep consult-project-extra
                consult-symbol csproj-mode dape denote dired-rsync
                dumb-jump eat editorconfig ef-themes eglot
                eglot-booster eldoc elisp-dev-mcp embark
                embark-consult erc exec-path-from-shell expreg faceup
                feature-mode flymake font-lock-ext forge gdscript-mode
                geiser geiser-guile gptel gptel-agent hl-todo idlwave
                jsonrpc just-mode magit marginalia markdown-ts-mode
                mcp mct mermaid-mode nerd-icons-completion
                nerd-icons-dired nerd-icons-grep nerd-icons-ibuffer
                nerd-icons-mode-line nerd-icons-modeline
                nerd-icons-xref no-littering nov ob-mermaid on
                orderless org org-edna pdf-tools peg prodigy project
                puni python rainbow-mode sbt-mode scala-repl
                scala-ts-mode sharper sln-mode sly sly-macrostep
                sly-overlay sly-quicklisp sql-indent track-changes
                tramp treesit-auto verb verilog-mode vertico web-mode
                wgrep which-key window-tool-bar winnow yaml-pro
                yasnippet yeetube))
 '(package-vc-selected-packages
   '((nerd-icons-mode-line :url
                           "https://github.com/grolongo/nerd-icons-mode-line.git")
     (eglot-booster :url "https://github.com/jdtsmith/eglot-booster")))
 '(project-switch-commands
   '((project-find-file "Find file") (consult-git-grep "Find regexp")
     (project-find-dir "Find directory")
     (magit-project-status "Version Control" "v")
     (project-eshell "EShell") (project-compile "Compile")))
 '(project-switch-use-entire-map t)
 '(read-extended-command-predicate 'command-completion-default-include-p)
 '(read-process-output-max 1048576 t)
 '(reb-re-syntax 'string)
 '(recentf-max-saved-items nil)
 '(remote-file-name-inhibit-auto-save-visited t)
 '(ring-bell-function 'ignore)
 '(safe-local-variable-values
   '((Log . clx.log) (Package . Xlib) (Lowercase . Yes) (Base . 10)
     (Package . XLIB) (Syntax . Common-lisp) (flymake-mode)))
 '(save-interprogram-paste-before-kill t)
 '(savehist-additional-variables '(search-ring regexp-search-ring kill-ring))
 '(scroll-conservatively 101 nil nil "Always scroll just enough text to bring point into view.")
 '(show-paren-when-point-inside-paren t)
 '(split-height-threshold nil)
 '(switch-to-buffer-obey-display-actions t)
 '(tab-always-indent 'complete)
 '(tab-width 4 nil nil "set default tab char's display width to 4 spaces")
 '(touch-screen-display-keyboard t)
 '(touch-screen-extend-selection t)
 '(touch-screen-preview-select t)
 '(touch-screen-word-select nil)
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
           tramp-file-name-regexp) nil nil "In addition to the usual files, also disable vc functionality in tramp files.")
 '(vertico-count 20)
 '(warning-minimum-level :error)
 '(wgrep-auto-save-buffer t)
 '(window-combination-resize t)
 '(yas-wrap-around-region t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

