(require 'package)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

(package-refresh-contents)

(let* ((packages '(no-littering
                   on
                   re-builder
                   consult-project-extra
                   ef-themes
                   yasnippet
                   puni
                   magit
                   dumb-jump
                   csproj-mode
                   fsharp-mode
                   scala-mode
                   sbt-mode
                   web-mode
                   dired-rsync
                   nerd-icons-dired
                   nerd-icons-completion
                   nerd-icons-grep
                   nerd-icons-xref
                   nerd-icons-ibuffer
                   consult-git-log-grep
                   prodigy
                   hl-todo
                   fennel-mode
                   geiser
                   geiser-guile
                   mermaid-mode
                   ob-mermaid
                   winnow
                   treesit-auto
                   yaml-mode
                   yaml-pro
                   avy
                   apheleia
                   stripspace
                   rainbow-mode
                   dape
                   auctex
                   orderless
                   ace-window
                   marginalia
                   consult
                   embark
                   embark-consult
                   cape
                   expreg
                   verb
                   sly
                   sly-overlay
                   sly-macrostep
                   sly-quicklisp
                   sql-indent
                   gptel
                   mcp
                   elisp-dev-mcp
                   gdscript-mode
                   eat
                   exec-path-from-shell
                   pdf-tools
                   nov
                   dictionary
                   google-translate)))
  (dolist (package packages)
    (package-install package)))

(package-vc-install "https://github.com/lem102/sln-mode")
(package-vc-install "https://github.com/grolongo/nerd-icons-mode-line.git")
(package-vc-install "https://github.com/jdtsmith/eglot-booster")
