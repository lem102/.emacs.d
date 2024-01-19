;;; init.el --- Jacob's main init file. -*-lexical-binding: t; eval: (flymake-mode 0)-*-
;;; Commentary:
;;; Code:

;; built-in
;; garbage collection

;; tweak garbage collection when using the minibuffer

(defun doom-defer-garbage-collection-h ()
  "Make Emacs wait as long as possible before garbage collecting."
  (setq gc-cons-threshold most-positive-fixnum))

(defun doom-restore-garbage-collection-h ()
  "Restore normal garbage collection."
  (run-at-time
   1 nil (lambda () (setq gc-cons-threshold 16777216))))

(add-hook 'minibuffer-setup-hook #'doom-defer-garbage-collection-h)
(add-hook 'minibuffer-exit-hook #'doom-restore-garbage-collection-h)


;; read environment file

(defvar jacob-font-size
  12 "Font size to use.")

(when (file-exists-p "~/.emacs.d/environment.el")
  (load-file "~/.emacs.d/environment.el"))


;; mouse config

(setq scroll-conservatively 101)
(setq mouse-wheel-progressive-speed nil)
(setq mouse-wheel-scroll-amount '(10 ((shift) . hscroll)
                                     ((meta))
                                     ((control) . text-scale)))


;; user interface config

(setq-default use-dialog-box nil)
(setq use-short-answers t)
(setq ring-bell-function 'ignore)
(setq truncate-partial-width-windows nil)
(setq-default truncate-lines nil)
(setq confirm-kill-processes nil)
(setq switch-to-buffer-obey-display-actions t)
(setq disabled-command-function nil)
(setq enable-recursive-minibuffers t)
(setq blink-cursor-blinks 0)            ; make cursor blink forever

(setq kill-buffer-query-functions (delq 'process-kill-buffer-query-function
                                        kill-buffer-query-functions))


;; screen sharing config

(define-minor-mode jacob-screen-sharing-mode
  "Minor mode for sharing screens."
  :global t
  :group 'jacob
  (let ((on (if jacob-screen-sharing-mode 1 0)))
    (global-hl-line-mode on)
    (global-display-line-numbers-mode on)))


;; backup/saving config

(setq create-lockfiles nil)
(setq history-length 1000)
(setq history-delete-duplicates t)
(setq backup-by-copying t)
(setq make-backup-files nil)
(setq auto-save-default nil)
(recentf-mode 1)

(setq savehist-file "~/.emacs.d/savehist")
(setq savehist-save-minibuffer-history t)
(savehist-mode 1)

(setq save-place-file "~/.emacs.d/saveplace")
(setq save-place-forget-unreadable-files t)
(save-place-mode 1)

(setq auto-save-visited-interval 2)
(setq auto-save-visited-predicate (lambda ()
                                    (not (equal major-mode 'message-mode))))
(auto-save-visited-mode 1)


;; misc config

(add-to-list 'load-path "~/.emacs.d/local-packages/")
(add-to-list 'load-path "~/.emacs.d/my-packages/")

;; (setq read-process-output-max (* 1024 1024))

(setq custom-file (make-temp-file "emacs-custom-"))

(setq inhibit-startup-screen t)

(setq split-height-threshold nil)

(setq parens-require-spaces nil)

(defvar jacob-welcome-messages '("\"A journey of a thousand miles begins with a single step.\" - 老子"
                                 "\"apex predator of grug is complexity\" - some grug"
                                 "\"Perfection is achieved, not when there is nothing more to add, but when there is nothing left to take away.\" - Antoine de Saint-Exupéry"
                                 "\"Always listen to Jiaqi.\" - Jacob Leeming")
  "List of messages to display in scratch buffer.")

(setq initial-scratch-message (format ";; %s\n\n"
                                      (seq-random-elt jacob-welcome-messages)))


;; bookmark config

(with-eval-after-load 'bookmark
  (setq bookmark-set-fringe-mark nil)
  (bookmark-store "init.el" '((filename . "~/.emacs.d/init.el")) nil)
  (bookmark-store "environment.el" '((filename . "~/.emacs.d/environment.el")) nil))


;; unicode

(prefer-coding-system 'utf-8)


;; help at point

(setq-default help-at-pt-display-when-idle '(flymake-diagnostic))
(help-at-pt-set-timer)


;; editing config

(global-subword-mode 1)

(setq show-paren-when-point-inside-paren t)
(show-paren-mode 1)

(electric-pair-mode 1)

(delete-selection-mode 1)

(repeat-mode 1)

(setq dabbrev-case-fold-search nil)
(setq dabbrev-case-replace nil)


;; mode line config

(setq display-time-day-and-date t)
(setq display-time-24hr-format t)
(display-time-mode 1)

(setq display-time-load-average-threshold 0)

(display-battery-mode 1)

(setq tab-bar-format '(tab-bar-format-align-right tab-bar-format-global))
(tab-bar-mode 1)

(column-number-mode 1)
(line-number-mode 1)

(defun jacob-mode-line-saved-status ()
  "Display symbol to show saved status of buffer."
  (cond ((buffer-modified-p (current-buffer)) "❌")
        (buffer-read-only "📚")
        (t "✓")))

(defvar jacob-mode-line-format
  '(" %e"
    mode-line-front-space
    (:eval (jacob-mode-line-saved-status))
    (:eval (pcase major-mode
             ('lisp-interaction-mode "ELi")
             ('emacs-lisp-mode "EL")
             ('nxml-mode "XML")
             (_ mode-name)))
    ": "
    "%b "
    (vc-mode vc-mode)
    " "
    mode-line-position
    (flymake-mode flymake-mode-line-format))
  "Custom mode line format.")

(setq-default mode-line-format jacob-mode-line-format)


;; tramp

;; tell vc to ignore tramp files
(setq vc-ignore-dir-regexp
      (format "\\(%s\\)\\|\\(%s\\)"
              vc-ignore-dir-regexp
              tramp-file-name-regexp))

;; lots of problems. for now, disable it!
(setq tramp-archive-enabled nil)


;; theme config

(setq ef-themes-to-toggle '(ef-autumn ef-cyprus))

(defun jacob-ef-theme ()
  "For use in `ef-themes-post-load-hook'."
  (with-eval-after-load 'pulse
    (modify-face 'pulse-highlight-start-face nil "magenta"))
  (with-eval-after-load 'org
    (set-face-attribute 'org-headline-done
                        nil
                        :strike-through t)))

(add-hook 'ef-themes-post-load-hook 'jacob-ef-theme)
(ef-themes-toggle)


;; js-mode config

(put 'js-indent-level 'safe-local-variable #'numberp)
(setq js-indent-level 2)


;; cc-mode config

(setq-default c-basic-offset 4)


;; csharp-mode config

(with-eval-after-load 'csharp-mode
  ;; mutate `csharp-ts-mode--indent-rules'
  (nconc (assoc 'c-sharp csharp-ts-mode--indent-rules)
         '(((parent-is "parameter_list") parent-bol csharp-ts-mode-indent-offset)
           ((parent-is "implicit_parameter_list") parent-bol csharp-ts-mode-indent-offset)
           ((parent-is "member_access_expression") parent-bol csharp-ts-mode-indent-offset)
           ((parent-is "lambda_expression") parent-bol csharp-ts-mode-indent-offset))))


;; dired-mode config

(with-eval-after-load 'dired
  (setq dired-recursive-copies 'always)
  (setq dired-dwim-target t)
  (setq delete-by-moving-to-trash t)

  (require 'ls-lisp)
  (setq ls-lisp-use-insert-directory-program nil)
  (setq ls-lisp-dirs-first t)
  (setq dired-listing-switches "-hal") ;; the h option needs to come first 🙃
  (setq dired-guess-shell-alist-user '(("\\.mkv\\'" "mpv")))

  (defun jacob-dired-mode-setup ()
    "Hook function for dired."
    (dired-hide-details-mode 1))

  (add-hook 'dired-mode-hook 'jacob-dired-mode-setup))


;; eshell config

(with-eval-after-load 'eshell
  (setq eshell-scroll-to-bottom-on-output t)

  (defun jacob-async-eshell-command ()
    "Run an async command through eshell."
    (interactive)
    (let* ((command (read-from-minibuffer "Emacs shell command: "))
           (dir (if (project-current)
                    (project-root (project-current))
                  default-directory))
           (buffer-name (concat "*" dir ":" command "*")))
      (kill-buffer buffer-name)
      (eshell-command (concat command " &"))
      (with-current-buffer (get-buffer "*Eshell Async Command Output*")
        (rename-buffer buffer-name))))

  (defun pcomplete/gco ()
    (pcomplete-here* (jacob-git-get-branches)))

  (defun pcomplete/grh ()
    (pcomplete-here* (jacob-git-get-branches t)))

  (defun jacob-git-get-branches (&optional display-origin)
    "Get git branches for current repo.

Non-nil DISPLAY-ORIGIN displays whether a branch is from origin, nil
hides this information."
    (with-temp-buffer
      (insert (shell-command-to-string "git branch -a"))
      (backward-delete-char 1)            ; delete rouge newline at end
      (goto-char (point-min))
      (flush-lines "->")
      (while (re-search-forward (if display-origin
                                    "remotes/"
                                  "remotes/origin/")
                                nil
                                t)
        (replace-match ""))
      (delete-rectangle (point-min) (progn
                                      (goto-char (point-max))
                                      (+ 2 (line-beginning-position))))
      (split-string (buffer-substring-no-properties (point-min) (point-max)) "\n"))))



;; flymake-mode config

(with-eval-after-load 'flymake
  (setq flymake-mode-line-format '(flymake-mode-line-exception flymake-mode-line-counters)))



;; project config

(with-eval-after-load 'project
  (setq project-switch-commands '((project-find-file "Find file")
                                  (jacob-project-search "Find regexp")
                                  (project-find-dir "Find directory")
                                  (project-vc-dir "VC-Dir")
                                  (project-eshell "Eshell"))))


;; emacs-lisp-mode config

(defun jacob-elisp-config-hook-function ()
  "Configure `emacs-lisp-mode' when hook run."
  (flymake-mode 1)
  (eldoc-mode 1))

(add-hook 'emacs-lisp-mode-hook 'jacob-elisp-config-hook-function)


;; font config

(set-frame-font (format "%s-%s"
                        (cdr (assoc-string system-type
                                           '(("windows-nt" . "Consolas")
                                             ("darwin" . "Menlo")
                                             ("gnu/linux" . "DejaVu Sans Mono"))))
                        jacob-font-size)
                t
                t)

;; enable emoji fonts
(set-fontset-font t
                  'emoji
                  (seq-find (lambda (font)
                              (member font (font-family-list)))
                            '("Symbola"
                              "Segoe UI Emoji"
                              "Noto Emoji"
                              "Noto Color Emoji"
                              "Apple Color Emoji")))


;; org config

;; this rebinds key in calendar mode unless set to nil, very annoying
(setq org-calendar-to-agenda-key nil)
(setq org-calendar-insert-diary-entry-key nil)

(add-hook 'org-mode-hook 'org-indent-mode)

(with-eval-after-load 'org
  (defun jacob-org-babel-tangle-delete-newline ()
    "Some code to get rid of the newline org babel likes to add
  in when it tangles into a file."
    (goto-char (point-max))
    (delete-trailing-whitespace)
    (backward-delete-char 1)
    (save-buffer))

  (add-hook 'org-babel-post-tangle-hook 'jacob-org-babel-tangle-delete-newline)

  (setq org-latex-pdf-process (list "latexmk -pdf %f -shell-escape")) ; probably requires texlive

  ;; for syntax highlighting in latex export. requires the minted
  ;; latex package, and pygmentize, a python package.
  (setq org-latex-listings 'minted)
  (add-to-list 'org-latex-packages-alist '("newfloat" "minted"))

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((octave . t)
     (sql . t)
     (js . t)))

  (setq org-src-preserve-indentation t)
  (setq org-confirm-babel-evaluate (lambda (lang body)
                                     (not (string= lang "restclient"))))
  (setq org-startup-folded t)
  (setq org-tags-column 0)

  (setq org-time-stamp-custom-formats (cons "%A %d/%m/%y" "%A %d/%m/%y %H:%M"))
  (setq org-display-custom-times t))


;; pulse config

(defun jacob-pulse-line (&rest _)
  "Pulse the current line."
  (pulse-momentary-highlight-region (save-excursion
                                      (back-to-indentation)
                                      (point))
                                    (line-end-position)))

(dolist (command '(
                   recenter-top-bottom
                   scroll-up-command
                   scroll-down-command
                   other-window
                   xref-find-definitions
                   xref-pop-marker-stack
                   isearch-done
                   ))
  (advice-add command :after #'jacob-pulse-line))


;; server config

(server-start)


;; time emacs startup

(add-hook 'emacs-startup-hook
          (lambda ()
            (message (emacs-init-time (concat "Emacs ready in %.2f seconds "
                                              (format "with %d garbage collections"
                                                      gcs-done))))))


;; TODO: consider removing
;; calendar + diary config

(with-eval-after-load 'calendar
  (setq diary-date-forms diary-european-date-forms)
  (setq calendar-date-style 'european)
  (setq calendar-date-display-form '((format "%02d/%02d/%04d" (string-to-number day) (string-to-number month) (string-to-number year))))
  (setq calendar-week-start-day 1)
  (setq calendar-mark-diary-entries-flag t)
  (setq calendar-mark-holidays-flag t)
  (add-hook 'calendar-today-visible-hook 'calendar-mark-today))


;; indentation config

;; use spaces to indent
(setq-default indent-tabs-mode nil)
;; set default tab char's display width to 4 spaces
(setq-default tab-width 4)
;; make tab key call indent command or insert tab character, depending on cursor position
(setq-default tab-always-indent 'complete)

(defun jacob-indent-with-major-mode ()
  "Indent buffer using current major mode.
Designed for use in on-save hook in certain programming languages modes."
  (unless (ignore-errors smerge-mode)
    (cond ((seq-contains-p '(
                             ;; csharp-tree-sitter-mode
                             ;; typescript-react-mode
                             java-mode
                             sml-mode)
                           major-mode
                           'string=)
           (indent-region (point-min) (point-max)))
          ((seq-contains-p '(emacs-lisp-mode
                             racket-mode
                             clojure-mode)
                           major-mode)
           (lisp-indent-region (point-min) (point-max))))))

(add-hook 'before-save-hook 'jacob-indent-with-major-mode)


;; microsoft windows config

(when (eq system-type 'windows-nt)
  (setq w32-pass-rwindow-to-system nil)
  (setq w32-rwindow-modifier 'super)
  (setq w32-apps-modifier 'hyper)

  (add-hook 'after-init-hook (lambda ()
                               ;; maximize window
                               (w32-send-sys-command 61488)))

  (defun jacob-confirm-terminate-batch-job ()
    "Type y and enter to terminate batch job after sending ^C."
    (when (not (null eshell-process-list))
      (insert "y")
      (eshell-send-input)))

  (advice-add 'eshell-interrupt-process :after #'jacob-confirm-terminate-batch-job))


;; winner-mode

(winner-mode 1)


;; sql config

(defun jacob-sql-login-hook ()
  "Custom SQL log-in behaviours.

See `sql-login-hook'."
  (when (eq sql-product 'postgres)
    (let ((proc (get-buffer-process (current-buffer))))
      (comint-send-string proc "\\set AUTOCOMMIT off\n"))))

(add-hook 'sql-login-hook 'jacob-sql-login-hook)

(defun jacob-sql-interactive-mode-hook ()
  "Custom interactive SQL mode behaviours.

See `sql-interactive-mode-hook' and `sql-product-alist'."
  (when (eq sql-product 'postgres)
    (setq sql-prompt-regexp "^[-[:alnum:]_]*[-=]\\*?[#>] ")
    (setq sql-prompt-cont-regexp "^\\(?:\\sw\\|\\s_\\)*[-(]\\*?[#>] ")))

(add-hook 'sql-interactive-mode-hook 'jacob-sql-interactive-mode-hook)


;; docview config

(with-eval-after-load 'doc-view-mode

  (defun jacob-doc-view-hook ()
    "hook function for doc view mode"
    (auto-revert-mode 1))

  (add-hook 'doc-view-mode-hook 'jacob-doc-view-hook)

  (when (eq system-type 'windows-nt)
    ;; To get these, install miktex.
    (setq doc-view-ghostscript-program "mgs.exe")
    (setq doc-view-pdf->png-converter-function 'doc-view-pdf->png-converter-ghostscript)
    (setq doc-view-pdftotext-program "miktex-pdftotext.exe")
    (setq doc-view-dvipdfm-program "dvipdfm.exe")
    ;; To get this, install LibreOffice.
    (setq doc-view-odf->pdf-converter-program "soffice.exe")
    (setq doc-view-odf->pdf-converter-function 'doc-view-odf->pdf-converter-soffice)))


;; compilation mode config

(setq compilation-always-kill t)
(setq compilation-scroll-output t)


;; treesit config

;; strategy for adopting tree-sitter:
;; on linux, use the auto build stuff included in emacs
;; on windows, grab the .dlls from a bundle

(when (eq system-type 'gnu/linux)
  (setq treesit-language-source-alist '((c-sharp "https://github.com/tree-sitter/tree-sitter-c-sharp" "master" "src")
                                        (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
                                        (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")))
  (setq treesit-load-name-override-list '((c-sharp "libtree-sitter-csharp" "tree_sitter_c_sharp")))
  ;; TODO: troubleshoot csharp-ts on windows
  (setq major-mode-remap-alist '((csharp-mode . csharp-ts-mode))))


;; eglot config

(setq eglot-ignored-server-capabilities '(:documentHighlightProvider
                                          :documentOnTypeFormattingProvider))

(defun jacob-remove-ret-character-from-buffer (&rest _)
  "Remove all occurances of ^M from the buffer.

Useful for deleting ^M after `eglot-code-actions'."
  (save-excursion
    (goto-char (point-min))
    (while (search-forward (char-to-string 13) nil t)
      (replace-match ""))))

(advice-add 'eglot-code-actions :after #'jacob-remove-ret-character-from-buffer)
(advice-add 'eglot-rename :after #'jacob-remove-ret-character-from-buffer)

(add-hook 'eglot-managed-mode-hook
          #'(lambda ()
              (eglot-inlay-hints-mode 0)
              (setq-local eldoc-documentation-strategy
                          'eldoc-documentation-compose)))

(add-hook 'java-mode-hook 'eglot-ensure)
(add-hook 'csharp-mode-hook 'eglot-ensure)
(add-hook 'csharp-ts-mode-hook 'eglot-ensure)
(add-hook 'typescript-mode-hook 'eglot-ensure)
(add-hook 'fsharp-mode-hook (lambda ()
                              (when (eq system-type 'gnu/linux)
                                (require 'eglot-fsharp)
                                (eglot-ensure))))
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs '((csharp-mode csharp-ts-mode) . ("csharp-ls")))

  (add-to-list 'eglot-server-programs '((js-mode typescript-mode) . ("typescript-language-server" "--stdio")))

  (add-to-list 'eglot-server-programs '(sql-mode . ("sql-language-server" "up" "--method" "stdio")))

  ;; (add-to-list 'eglot-server-programs '((js-mode typescript-mode) . (eglot-deno "deno" "lsp")))

  (defclass eglot-deno (eglot-lsp-server) ()
    :documentation "A custom class for deno lsp.")

  (cl-defmethod eglot-initialization-options ((server eglot-deno))
    "Passes through required deno initialization options"
    (list :enable t
          :lint t
          :suggest.names t))

  ;; (add-to-list 'eglot-server-programs '(go-mode . ("/home/jacob/go/bin/gopls")))

  (eglot--code-action eglot-code-action-organize-imports-ts "source.organizeImports.ts")
  (eglot--code-action eglot-code-action-add-missing-imports-ts "source.addMissingImports.ts")

  (defun eglot--format-markup (markup)
    "Format MARKUP according to LSP's spec."
    (pcase-let ((is-csharp (equal 'csharp-mode-hook major-mode))
                (`(,string ,mode)
                 (if (stringp markup) (list markup 'gfm-view-mode)
                   (list (plist-get markup :value)
                         (pcase (plist-get markup :kind)
                           ;; changed this line, before was gfm-view-mode instead of markdown-view-mode
                           ("markdown" 'markdown-view-mode)
                           ("plaintext" 'text-mode)
                           (_ major-mode))))))
      (with-temp-buffer
        (switch-to-buffer (current-buffer))
        (setq-local markdown-fontify-code-blocks-natively t)
        (insert string)
        (let ((inhibit-message t)
	          (message-log-max nil))
          (ignore-errors (delay-mode-hooks (funcall mode))))
        (font-lock-ensure)
        (string-trim (buffer-string))))))


;; typescript config

(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode))


;; message config

(with-eval-after-load 'message
  (setq message-send-mail-function 'smtpmail-send-it))


;; gnus config

(with-eval-after-load 'gnus
  (setq gnus-use-full-window nil)
  (setq gnus-always-read-dribble-file t)
  (gnus-demon-add-handler 'gnus-demon-scan-news 2 t)
  (add-hook 'gnus-after-getting-new-news-hook 'gnus-notifications))



;; package configuration

(require 'package)

(defmacro jacob-is-installed (package &rest body)
  "If PACKAGE is installed, evaluate BODY.
Used when attempting to lazy load PACKAGE."
  (declare (indent 1))
  `(when (package-installed-p ,package)
     ,@body))

(defmacro jacob-try-require (feature &rest body)
  "Attempt to require FEATURE.

If successful, evaluate BODY.  Used to eagerly load feature."
  (declare (indent 1))
  `(when (require ,feature nil 'noerror)
     ,@body))

(setq package-archives '(
                         ("GNU" . "https://elpa.gnu.org/packages/")
                         ("non-GNU" . "https://elpa.nongnu.org/nongnu/")
                         ("melpa" . "https://melpa.org/packages/")
                         ))


;; slack config

(unless (package-installed-p 'slack)
  (package-vc-install '(slack . (:url "https://github.com/lem102/emacs-slack.git"))))

(with-eval-after-load 'slack
  (setq slack-enable-global-mode-string t)
  (setq slack-buffer-emojify t)
  (setq slack-prefer-current-team t)
  (setq slack-thread-also-send-to-room nil)

  (defun jacob-slack-hook-function ()
    "Function to be run in slack mode hooks."
    (toggle-word-wrap 1))

  (add-hook 'slack-message-buffer-mode-hook 'jacob-slack-hook-function))


;; dape config

(with-eval-after-load 'dape
  (setq dape-info-hide-mode-line nil)
  (setq dape-buffer-window-arrangment 'right)

  (pop dape-configs)

  ;; (netcoredbg modes
  ;;            (csharp-mode csharp-ts-mode)
  ;;            ensure dape-ensure-command command "netcoredbg" command-args
  ;;            ["--interpreter=vscode"]
  ;;            :request "launch" :cwd dape-cwd-fn :program dape-find-file :stopAtEntry t)

  (push '(netcoredbg-attach-port modes (csharp-mode csharp-ts-mode)
                                 ensure dape-ensure-command
                                 command "netcoredbg"
                                 command-args ["--interpreter=vscode"]
                                 :request "attach"
                                 :cwd dape-cwd-fn
                                 :program dape-find-file
                                 :stopAtEntry t
                                 :processId dape-read-pid)
        dape-configs
        ))



;; switch-window configuration

;; JACOBTODO: investigate switch window finish hook to solve compilation scroll issue
(jacob-try-require 'switch-window
  (setq switch-window-shortcut-style 'qwerty)
  (setq switch-window-threshold 3))


;; racket-mode

(jacob-is-installed 'racket-mode
  (add-hook 'racket-mode-hook 'racket-xp-mode))


;; auctex

(jacob-is-installed 'auctex
  (with-eval-after-load 'auctex
    (setq TeX-auto-save t)
    (setq TeX-parse-self t)
    (setq-default japanese-TeX-error-messages nil)
    (TeX-global-PDF-mode 0)))


;; restclient

(jacob-is-installed 'restclient
  (add-to-list 'auto-mode-alist '("\\.http\\'" . restclient-mode)))



(jacob-is-installed 'fsharp-mode
  (with-eval-after-load 'fsharp-mode
    (setq inferior-fsharp-program "dotnet fsi --fsi-server-input-codepage:65001")))



(jacob-is-installed 'purescript-mode
  (with-eval-after-load 'purescript-mode
    (add-hook 'purescript-mode-hook 'turn-on-purescript-indentation)))



;; kotlin-mode config

(jacob-is-installed 'kotlin-mode
  (with-eval-after-load 'kotlin-mode

    (define-skeleton jacob-kotlin-test
      "Insert kotlin function"
      > "@Test" \n
      "fun " - "() {" \n
      \n
      -4 "}")

    (define-skeleton jacob-kotlin-function
      "Insert kotlin function"
      > "fun " - "() {" \n
      -4 \n
      -4 "}")

    (define-skeleton jacob-kotlin-val
      "Insert kotlin val"
      > "val " - " = ")

    (define-skeleton jacob-kotlin-println
      "Insert kotlin println"
      > "println(" - ")")

    (define-skeleton jacob-kotlin-when
      "Insert kotlin when"
      > "when (" - ") {" \n
      "else -> " \n
      -4 "}")

    (define-skeleton jacob-kotlin-list
      "Insert kotlin list"
      > "listOf(" - ")")))



(jacob-try-require 'orderless
  (setq completion-styles '(orderless initials)))



(jacob-try-require 'vertico
  ;; TODO: attempt to make number of candidates equal to 1/4 of screen
  (setq vertico-count 25)
  (vertico-mode 1))



(jacob-try-require 'marginalia
  (marginalia-mode 1))


;; consult config

(jacob-is-installed 'consult
  (setq completion-in-region-function 'consult-completion-in-region)

  (setq consult-preview-raw-size 0)

  (setq consult-project-root-function
        (lambda ()
          (when-let (project (project-current))
            (car (project-roots project)))))

  (setq xref-show-xrefs-function 'consult-xref)
  (setq xref-show-definitions-function 'consult-xref))



(jacob-is-installed 'expand-region
  (with-eval-after-load 'expand-region
    (setq expand-region-contract-fast-key "9")))



(jacob-is-installed 'sml-mode
  (with-eval-after-load 'sml-mode

    (setq sml-abbrev-skeletons nil)

    (define-skeleton jacob-sml-skeleton-val
      "insert val" nil
      > "val " - " =")

    (define-skeleton jacob-sml-skeleton-if
      "insert if" nil
      > "if " - "" \n
      -4 "then " \n
      -4 "else ")

    (define-skeleton jacob-sml-skeleton-let
      "insert let" nil
      > "let" \n
      - \n
      -4 "in" \n
      -4 "end")

    (define-skeleton jacob-sml-skeleton-function
      "insert function" nil
      > "fun " - " =")

    (define-skeleton jacob-sml-skeleton-anonymous-function
      "insert anonymous functionction" nil
      > "fn " - " => ")

    (define-skeleton jacob-sml-skeleton-case
      "insert case" nil
      > "case " - " of" \n
      " => ")))


;; xah-fly-keys config

(setq xah-fly-use-control-key nil)
(setq xah-fly-use-meta-key nil)

(jacob-try-require 'xah-fly-keys
  (xah-fly-keys-set-layout "qwerty")
  (xah-fly-keys 1))



;; personal functions

(defun jacob-insert-elisp-colour ()
  "Ask user for a colour, insert colour name at point."
  (interactive)
  (insert (concat "\"" (read-color) "\"")))

(defun jacob-eval-print-last-sexp ()
  "Run `eval-print-last-sexp', indent the result."
  (interactive)
  (save-excursion
    (eval-print-last-sexp 0))
  (save-excursion
    (forward-line)
    (indent-pp-sexp t)))

(defun jacob-system-free-space ()
  "Display free space on system storage.
Should work cross platform."
  (interactive)
  (shell-command (pcase system-type
                   ('gnu/linux "df -h")
                   ('windows-nt "powershell -Command Get-Volume"))))

(defun jacob-alist-to-form-data (alist)
  "Convert ALIST to form-data for http request."
  (mapconcat (lambda (x)
               (concat (car x) "=" (cdr x)))
             alist
             "&"))

(defun jacob-format-xml ()
  "Format xml on current line."
  (insert (let ((input (prog1
                           (buffer-substring-no-properties (line-beginning-position) (line-end-position))
                         (delete-region (line-beginning-position) (line-end-position)))))
            (with-temp-buffer
              (insert input)
              (goto-char (point-min))
              (while (search-forward-regexp "\>[ \\t]*\<" nil t)
                (backward-char) (insert "\n"))
              (nxml-mode)
              (indent-region (point-min) (point-max))
              (buffer-substring-no-properties (point-min) (point-max))))))

;; TODO: remove web request helper code, replace with either request.el, or pls.el packages.

(defun jacob-web-request-helper (url &optional method headers data data-format-function data-parse)
  "Helper function for making web requests.
METHOD, HEADERS and DATA are for the corresponding url-request variables.
URL is the address to send the request to.
Returns a string containing the response.

DATA-FORMAT-FUNCTION is a function that takes one argument and returns
DATA in string form.

DATA-PARSE is a symbol specifying the output of this function.  If not
given, it will return the http reponse in string form.  If `json' it
will return the json data as a Lisp object."
  (require 'json)
  (with-current-buffer (let ((url-request-method (if (null method)
                                                     "GET"
                                                   method))
                             (url-request-extra-headers (if (equal data-format-function 'json-encode)
                                                            (cons '("content-type" . "application/json") headers)
                                                          headers))
                             (url-request-data (if (null data-format-function)
                                                   data
                                                 (funcall data-format-function data))))
                         (url-retrieve-synchronously url))
    (goto-char (point-min))
    (when (search-forward-regexp "Content-Type: application/[a-z+]*json" nil t)
      (search-forward "\n\n" nil t)
      (json-reformat-region (point) (point-max)))
    (when (search-forward-regexp "Content-Type:.*xml" nil t)
      (search-forward "\n\n" nil t)
      (jacob-format-xml))
    (goto-char (point-min))
    (pcase data-parse
      ('json (progn
               (search-forward "\n\n" nil t)
               (json-read)))
      (_ (buffer-string)))))

(defun jacob-goto-pi ()
  "Connect to raspberry pi."
  (interactive)
  (find-file jacob-raspberry-pi-connection-string))

(defun jacob-toggle-modeline ()
  "Toggle visibility of modeline."
  (interactive)
  (setq mode-line-format (if (null mode-line-format)
                             jacob-mode-line-format
                           nil)))

(jacob-is-installed 'consult
  (defun jacob-project-search ()
    "If current project is a git project, use consult git grep, otherwise use consult grep."
    (interactive)
    (if (vc-find-root default-directory ".git")
        (consult-git-grep)
      (consult-grep))))

(defun jacob-async-shell-command (command)
  "Wrapper command for (`async-shell-command' COMMAND)."
  (interactive
   (list
    (read-shell-command (if shell-command-prompt-show-cwd
                            (format-message "Async shell command in `%s': "
                                            (abbreviate-file-name
                                             default-directory))
                          "Async shell command: ")
                        nil nil
			            (let ((filename
			                   (cond
				                (buffer-file-name)
				                ((eq major-mode 'dired-mode)
				                 (dired-get-filename nil t)))))
			              (and filename (file-relative-name filename))))))
  (async-shell-command command
                       (concat "* " default-directory " " command " *")))

(defvar jacob-format-words-style-and-start nil
  "Pair of currently selected style and starting point.
If nil, means you havent used the command for the first time yet.")

(defun jacob-format-words ()
  "Command for formating words into identifiers when writing code.

On first use, ask for formatting style (e.g. kebab, camel,
etc).  Format one word backwards in selected style and store the style
and the position of point after formatting, return point to where it
was when command called.

On consecutive use, apply stored formatting to word before stored
point."
  (interactive)

  (undo-boundary)

  (unless (eq last-command this-command)
    (setq jacob-format-words-3-style-and-start (cons (read-char-from-minibuffer "select style: " '(?c ?p ?k ?s ?S))
                                                     (point))))

  (save-excursion
    (let* ((style (car jacob-format-words-3-style-and-start))
           (format-position (cdr jacob-format-words-3-style-and-start))
           (bounds (progn
                     (goto-char format-position)
                     (bounds-of-thing-at-point 'word)))
           (start (car bounds))
           (end (cdr bounds)))
      (pcase style
        (?c (progn
              (backward-word)
              (capitalize-word 1)
              (backward-word)
              (backward-delete-char 1)))
        (?p (progn
              (backward-word)
              (capitalize-word 1)
              (backward-word)
              (backward-delete-char 1)
              (backward-word)
              (capitalize-word 1)))
        (?k (progn
              (backward-word)
              (backward-delete-char 1)
              (insert-char ?-)
              (backward-char)))
        (?s (progn
              (backward-word)
              (backward-delete-char 1)
              (insert-char ?_)
              (backward-char)))
        (?S (progn
              (backward-word)
              (upcase-word 1)
              (backward-word)
              (backward-delete-char 1)
              (insert-char ?_)
              (backward-char)
              (backward-word)
              (upcase-word 1))))

      (setq jacob-format-words-3-style-and-start (cons style (point))))))

(defun jacob-count-words-region ()
  "If mark active count words in region, otherwise count words in whole buffer."
  (interactive)
  (if mark-active
      (call-interactively 'count-words-region)
    (let ((current-prefix-arg t))
      (call-interactively 'count-words-region))))

(define-key global-map (kbd "M-=") 'jacob-count-words-region)

(defun jacob-eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))

(require 'jacob-long-time-autoloads)

(defun jacob-insert-plus ()
  "Insert +."
  (interactive)
  (insert "+"))

(defun jacob-insert-equals ()
  "Insert =."
  (interactive)
  (insert "="))

(defun jacob-insert-apostrophe ()
  "Insert '."
  (interactive)
  (insert "'"))

(defun jacob-insert-at ()
  "Insert @."
  (interactive)
  (insert "@"))

(defun jacob-insert-tilde ()
  "Insert ~."
  (interactive)
  (insert "~"))

(defun jacob-insert-hash ()
  "Insert #."
  (interactive)
  (insert "#"))

(defun jacob-insert-exclamation-mark ()
  "Insert !."
  (interactive)
  (insert "!"))

(defun jacob-insert-pound-sign ()
  "Insert £."
  (interactive)
  (insert "£"))

(defun jacob-insert-dollar-sign ()
  "Insert $."
  (interactive)
  (insert "$"))

(defun jacob-insert-percentage-sign ()
  "Insert %."
  (interactive)
  (insert "%"))

(defun jacob-insert-caret ()
  "Insert ^."
  (interactive)
  (insert "^"))

(defun jacob-insert-ampersand ()
  "Insert &."
  (interactive)
  (insert "&"))

(defun jacob-insert-asterisk ()
  "Insert *."
  (interactive)
  (insert "*"))

(defun josh-kill-process-on-port ()
  "Ask for a port, kill process on that port.  For powershell."
  (interactive)
  (shell-command (concat "powershell.exe -File %home%\\Downloads\\Jacob.ps1 -localPort " (read-from-minibuffer "port: "))))

(require 'jacob-play-youtube-autoloads)

(defun jacob-lookup-wikipedia ()
  "Ask for a string to search.
Search youtube for string and display in browser."
  (interactive)
  (let ((search-query (read-from-minibuffer "Wikipedia: ")))
    (browse-url (concat "https://en.wikipedia.org/wiki/" search-query))))

(defun jacob-bookmark-jump-to-url (bookmark)
  "Open link stored in the filename property of BOOKMARK in browser."
  (browse-url (cdr (assoc 'filename (cdr bookmark)))))

(defun jacob-swap-visible-buffers ()
  "If two windows in current frame, swap their buffers.
Otherwise, display error message."
  (interactive)
  (if (length= (window-list) 2)
      (let* ((current-window (car (window-list)))
             (other-window (cadr (window-list)))
             (current-buffer (window-buffer current-window))
             (other-buffer (window-buffer other-window)))
        (set-window-buffer current-window other-buffer)
        (set-window-buffer other-window current-buffer)
        (other-window 1))
    (message "More/less than 2 windows in frame.")))

(defun jacob-clear-highlighted-regex ()
  "Remove all highlighting."
  (interactive)
  (unhighlight-regexp t))

(defun jacob-npm-project-p ()
  "Return non-nil if current project is an npm project."
  (seq-find (lambda (x)
              (string= x "package.json"))
            (directory-files (project-root (project-current)))))

(defun jacob-format-buffer-shell-command (command)
  "Run shell command COMMAND to format the current file, then revert the buffer."
  (save-buffer)
  (shell-command (format command
                         (shell-quote-argument buffer-file-name)))
  (revert-buffer t t t))

(defun jacob-format-buffer ()
  "Format the current buffer."
  (interactive)
  (pcase major-mode
    ((or 'typescript-react-mode 'js-mode) (progn
                                            (ignore-errors (eglot-code-action-add-missing-imports-ts (point-min) (point-max)))
                                            (eglot-code-action-organize-imports-ts (point-min) (point-max))
                                            (jacob-format-buffer-shell-command (if (jacob-npm-project-p)
                                                                                   "prettier %s -w"
                                                                                 "deno fmt %s"))))
    ('go-mode (jacob-format-buffer-shell-command "gofmt -w %s"))
    (t (message "no formatting specified"))))

;; FIXME: keys that are not already bound will not work for jacob-xfk-define-key-in-major-mode
(defun jacob-xfk-define-key-in-major-mode (major-mode-keymap key command)
  "In MAJOR-MODE-KEYMAP bind KEY to COMMAND only when in xfk command mode."
  (define-key major-mode-keymap (vector 'remap (lookup-key xah-fly-command-map key)) command))

(defun jacob-git-push-set-upstream ()
  "Push current git branch to new upstream branch."
  (interactive)
  (shell-command "git push --set-upstream origin HEAD"))

(defvar jacob-vc-main-branch nil
  "default main branch")

(defun jacob-git-pull-master-new-branch ()
  "Update main branch defined in `jacob-vc-main-branch'.
Prompt for branch name."
  (interactive)
  (when (zerop (shell-command (format "git checkout %s" jacob-vc-main-branch)))
    (when (zerop (shell-command "git pull"))
      (shell-command (concat "git checkout -b " (read-from-minibuffer "branch name: "))))))

(defun jacob-npm-fix-linting ()
  "Fix linting for npm then commit and push."
  (interactive)
  (when (zerop (shell-command "npm run lint:fix"))
    (shell-command "git commit -m \"chore: fix linting\" -na")
    (shell-command "git push")))

;; TODO: current-line and current-column come from array.el, so this function will not work until after that lib is loaded
(defun jacob-open-in-vscode ()
  "Open current file in vscode."
  (interactive)
  (let ((default-directory (project-root (project-current)))
        (file (buffer-file-name))
        (line (number-to-string (+ (current-line) 1)))
        (column (number-to-string (+ (current-column) 1))))
    (shell-command (concat "code . --reuse-window --goto \"" file "\":" line ":" column))))

(defun jacob-toggle-mocha-only ()
  "Toggle the presence of .only after an it/describe mocha test."
  (interactive)
  (save-excursion
    (backward-up-list)
    (re-search-backward (rx (or (one-or-more blank)
                                line-start)
                            (or "it"
                                "describe")))
    (forward-word)
    (if (string-match "\\.only" (thing-at-point 'line t))
        (kill-word 1)
      (insert ".only"))))

(defun jacob-toggle-test-category ()
  "Toggle the presence of a test category attribute after a mstest unit test."
  (interactive)
  (save-excursion
    (search-backward "[TestMethod]")
    (forward-to-indentation 1)
    (if (string-match "\\[TestCategory(\"MyCategory\")\\]" (thing-at-point 'line t))
        (kill-line 1)
      (insert "[TestCategory(\"MyCategory\")]\n"))))

(defvar jacob-git-lab-push-set-upstream-jira-url ""
  "URL for current employer's jira board.")

(defun jacob-git-lab-push-set-upstream ()
  "Push the current branch and create an upstream branch.
Use GitLab push options to create a merge request and set all
necessary values.

For use with GitLab only."
  ;; TODO: auto-update jira ticket with MR link
  (interactive)
  (let* ((branch-name (with-temp-buffer
                        (eshell-command "git symbolic-ref HEAD --short" t)
                        (buffer-substring-no-properties (point-min) (- (point-max) 1))))
         (mr-key (progn
                   (string-match (rx (+ alpha) "-" (+ digit))
                                 branch-name)
                   (match-string 0 branch-name)))
         (jira-link (concat jacob-git-lab-push-set-upstream-jira-url mr-key))
         (command (concat "git push --set-upstream origin HEAD "
                          (concat "-o merge_request.create "
                                  "-o merge_request.remove_source_branch "
                                  (concat "-o merge_request.title=\""
                                          branch-name
                                          "\" ")
                                  (concat "-o merge_request.description=\""
                                          "[" mr-key "](" jira-link ")"
                                          "\" "))))
         (mr-link (with-temp-buffer
                    (switch-to-buffer (current-buffer))
                    (eshell-command command t)
                    (goto-char (point-min))
                    (search-forward "https")
                    (thing-at-point 'url))))
    (kill-new (concat mr-key "\n"
                      mr-link "\n"
                      jira-link))
    (browse-url mr-link)))

(defun jacob-next-error-or-punct ()
  "Wrapper command to allow moving forward by error or punctuation."
  (interactive)
  (if flymake-mode
      (flymake-goto-next-error 1 nil t)
    (xah-forward-punct)))

(defun jacob-previous-error-or-punct ()
  "Wrapper command to allow moving backward by error or punctuation."
  (interactive)
  (if flymake-mode
      (flymake-goto-prev-error 1 nil t)
    (xah-backward-punct)))



;; abbrevs


;; abbrev config

(setq abbrev-suggest t)
(set-default 'abbrev-mode t)
(setq save-abbrevs nil)


;; jacob-insert-config

(defun jacob-insert-block-helper (template)
  "Call `jacob-insert-helper' with a block appended to TEMPLATE."
  (jacob-insert-helper (concat template " {\n●\n}")))

(defun jacob-insert-assignment-helper (template)
  "Call `jacob-insert-helper' with \" ■ = \" appended to TEMPLATE."
  (jacob-insert-helper (concat template " ■ = ●")))

(defun jacob-insert-js-function-helper (template)
  "Call `jacob-insert-helper' and insert js anonymous function inside of TEMPLATE."
  (jacob-insert-helper (concat template "((■) => ●)")))

(defun jacob-insert-helper (template)
  "Insert TEMPLATE in current buffer.
If present, set mark at each ● so that going throught the mark ring
will take the user from first instance to last and delete it.  If
present, move point back to ■ and delete it."
  (let* ((●-positions '())
         (■-position)
         (start (prog1
                    (point)
                  (insert template)))
         (end (point)))

    (goto-char start)
    (dotimes (_ (- (+ 1 (line-number-at-pos end)) (line-number-at-pos (point))))
      (ignore-errors (indent-according-to-mode))

      (goto-char (line-beginning-position))
      (while (search-forward-regexp (rx (or ?■ ?●)) (line-end-position) t)
        (let ((match (match-string-no-properties 0)))
          (backward-delete-char 1)
          (if (string= match "■")
              (setq ■-position (point))
            (setq ●-positions (cons (point) ●-positions)))))

      (forward-line 1))

    (mapc (lambda (position)
            (goto-char position)
            (push-mark))
          ●-positions)

    (goto-char ■-position)

    ;; return t to prevent self insert when calling from abbrev.
    t))

(defmacro define-jacob-insert (name insert)
  "Define a jacob-insert command called NAME.
Calls INSERT."
  (declare (indent 1))
  `(progn
     ;; prevent abbrev from self-inserting
     (put ',name 'no-self-insert t)
     (defun ,name ()
       ,insert)))

(define-jacob-insert jacob-insert-c-if
  (jacob-insert-block-helper "if (■)"))

(define-jacob-insert jacob-insert-c-while
  (jacob-insert-block-helper "while (■)"))

(define-jacob-insert jacob-insert-c-for
  (jacob-insert-block-helper "for (■)"))

(define-jacob-insert jacob-insert-java-for-each
  (jacob-insert-block-helper "for (var ■ : ●)"))

(define-jacob-insert jacob-insert-java-class
  (jacob-insert-block-helper "●class ■"))

(define-jacob-insert jacob-insert-java-method
  (jacob-insert-block-helper "■(●)"))

(define-jacob-insert jacob-insert-java-constructor
  (jacob-insert-block-helper (concat (save-excursion
                                       (when (search-backward-regexp (rx "class "
                                                                         (group (one-or-more (any "a-zA-Z"))))
                                                                     nil
                                                                     t)
                                         (match-string-no-properties 1)))
                                     "■(●)")))

(define-jacob-insert jacob-insert-c-switch
  (jacob-insert-block-helper "switch (■)"))

(define-jacob-insert jacob-insert-c-case
  (jacob-insert-helper "case ■: \n●\nbreak;"))

(define-jacob-insert jacob-insert-java-main
  (jacob-insert-block-helper "public static void main(String[] args)"))

(define-jacob-insert jacob-insert-java-print
  (jacob-insert-helper "System.out.println(■);"))

(define-jacob-insert jacob-insert-java-var
  (jacob-insert-assignment-helper "var"))

(define-jacob-insert jacob-insert-elisp-goto-char
  (jacob-insert-helper "(goto-char ■)"))

(define-jacob-insert jacob-insert-elisp-with-eval-after-load
  (jacob-insert-helper "(with-eval-after-load ■)"))

(define-jacob-insert jacob-insert-lisp-let
  (jacob-insert-helper "(let ((■))\n●)"))

(define-jacob-insert jacob-insert-elisp-defun
  (jacob-insert-helper "(defun ■ (●)\n●)"))

(define-jacob-insert jacob-insert-elisp-message
  (jacob-insert-helper "(message \"%s\" ■)"))

(define-jacob-insert jacob-insert-lisp-cond
  (jacob-insert-helper "(cond ((■))\n●)"))

(define-jacob-insert jacob-insert-js-print
  (jacob-insert-helper "console.log(■);"))

(define-jacob-insert jacob-insert-js-const
  (jacob-insert-assignment-helper "const"))

(define-jacob-insert jacob-insert-js-let
  (jacob-insert-assignment-helper "let"))

(define-jacob-insert jacob-insert-js-function
  (jacob-insert-helper "(■) => ●"))

(define-jacob-insert jacob-insert-js-for-each
  (jacob-insert-assignment-helper "forEach(■)"))

(define-jacob-insert jacob-insert-js-describe
  (jacob-insert-helper "describe(\"■\", () => {\n●\n});"))

(define-jacob-insert jacob-insert-js-it
  (jacob-insert-helper "it(\"■\", () => {\n●\n});"))

(define-jacob-insert jacob-insert-js-map-function
  (jacob-insert-js-function-helper "map"))

(define-jacob-insert jacob-insert-js-filter-function
  (jacob-insert-js-function-helper "filter"))

(define-jacob-insert jacob-insert-go-println
  (jacob-insert-helper "fmt.Println(■)"))

(define-jacob-insert jacob-insert-go-printf
  (jacob-insert-helper "fmt.Printf(■)"))

(define-jacob-insert jacob-insert-go-func
  (jacob-insert-block-helper "func ■()"))

(define-jacob-insert jacob-insert-go-for
  (jacob-insert-block-helper "for ■"))

(define-jacob-insert jacob-insert-go-for-range
  (jacob-insert-block-helper "for i, v := range ■"))

(define-jacob-insert jacob-insert-go-if
  (jacob-insert-block-helper "if ■"))

(define-jacob-insert jacob-insert-go-struct
  (jacob-insert-block-helper "struct"))

(define-jacob-insert jacob-insert-csharp-print
  (jacob-insert-helper "Console.WriteLine(■);"))

(define-jacob-insert jacob-insert-csharp-property
  (jacob-insert-helper "■ { get; set; }"))

(define-jacob-insert jacob-insert-clojure-defn
  (jacob-insert-helper "(defn ■ [●]\n●)"))

(define-jacob-insert jacob-insert-clojure-loop
  (jacob-insert-helper "(loop [■]\n●)"))

(define-jacob-insert jacob-insert-clojure-recur
  (jacob-insert-helper "(recur ■)"))

(define-jacob-insert jacob-insert-clojure-let
  (jacob-insert-helper "(let [■]\n●)"))

(define-jacob-insert jacob-insert-clojure-if
  (jacob-insert-helper "(if ■)"))

(define-jacob-insert jacob-insert-clojure-case
  (jacob-insert-helper "(case ■\n●)"))

(defun jacob-abbrev-expand-function ()
  "Return t if not in string or comment. Else nil."
  (let ((xsyntax-state (syntax-ppss)))
    (not (or (nth 3 xsyntax-state) (nth 4 xsyntax-state)))))

(define-abbrev-table 'global-abbrev-table
  '(
    ("dal" "$")
    ;; ("eq" "=")
    ("eeq" "==")
    ("eeeq" "===")
    ("sco" "_")
    ))

(define-abbrev-table 'text-mode-abbrev-table
  '(("i" "I")
    ("im" "I'm")
    ("id" "I'd")
    ("dont" "don't")
    ("its" "it's")
    ("havent" "haven't")))

(define-abbrev-table 'common-operators-abbrev-table
  '(("lt" "<")
    ("gt" ">")
    ("lte" "<=")
    ("gte" ">=")
    ("eq" "==")
    ("neq" "!=")
    ("or" "||")
    ("and" "&&")
    ("ret" "return"))
  :enable-function 'jacob-abbrev-expand-function)

(define-abbrev-table 'c-mode-abbrev-table
  '(
    ("if" "" jacob-insert-c-if)
    ("for" "" jacob-insert-c-for)
    ("while" "" jacob-insert-c-while)
    ("switch" "" jacob-insert-c-switch)
    ("case" "" jacob-insert-c-case))
  nil
  :parents (list common-operators-abbrev-table)
  :enable-function 'jacob-abbrev-expand-function)

(define-abbrev-table 'js-mode-abbrev-table
  '(
    ("cl" "" jacob-insert-js-print)
    ("fun" "" jacob-insert-js-function)
    ("con" "" jacob-insert-js-const)
    ("let" "" jacob-insert-js-let)
    ("eq" "===")
    ("neq" "!==")
    ("ret" "return")
    ("fore" "" jacob-insert-js-for-each)
    ("jwe" "console.log(\"jacobwozere\");" t)
    ("desc" "" jacob-insert-js-describe)
    ("it" "" jacob-insert-js-it)
    ("map" "" jacob-insert-js-map-function)
    ("filter" "" jacob-insert-js-filter-function))
  nil
  :parents (list c-mode-abbrev-table))

(define-abbrev-table 'js-ts-mode-abbrev-table
  nil
  nil
  :parents (list js-mode-abbrev-table))

(define-abbrev-table 'typescript-ts-mode-abbrev-table
  nil
  nil
  :parents (list js-mode-abbrev-table))

(define-abbrev-table 'tsx-ts-mode-abbrev-table
  nil
  nil
  :parents (list js-mode-abbrev-table))

(define-abbrev-table 'common-java-csharp-abbrev-table
  '(("pri" "private")
    ("pub" "public")
    ("sta" "static")
    ("class" "" jacob-insert-java-class)
    ("cons" "" jacob-insert-java-constructor)
    ("var" "" jacob-insert-java-var)
    ("meth" "" jacob-insert-java-method))
  nil
  :parents (list c-mode-abbrev-table)
  :enable-function 'jacob-abbrev-expand-function)

(define-abbrev-table 'java-mode-abbrev-table
  '(("sout" "" jacob-insert-java-print)
    ("psvm" "" jacob-insert-java-main)
    ("fore" "" jacob-insert-java-for-each)
    ("string" "String")
    ("double" "Double")
    ("object" "Object")
    ("fin" "final")
    ("char" "Character")
    ("inst" "instanceof"))
  nil
  :parents (list common-java-csharp-abbrev-table))

(define-abbrev-table 'common-csharp-abbrev-table
  '(("cwl" "" jacob-insert-csharp-print)
    ("jwe" "Console.WriteLine(\"jacobwozere\");" t)
    ("as" "async")
    ("ns" "namespace")
    ("guid" "Guid")
    ("prop" "" jacob-insert-csharp-property)
    ("xgon" "x => x" t)
    ("ro" "readonly")
    ("guid" "Guid.NewGuid()" t)
    ("tostr" "ToString()" t))
  nil
  :parents (list common-java-csharp-abbrev-table))

(define-abbrev-table 'csharp-mode-abbrev-table
  nil
  nil
  :parents (list common-csharp-abbrev-table))

(define-abbrev-table 'csharp-ts-mode-abbrev-table
  nil
  nil
  :parents (list common-csharp-abbrev-table))

(define-abbrev-table 'emacs-lisp-mode-abbrev-table
  '(("def" "" jacob-insert-elisp-defun)
    ("let" "" jacob-insert-lisp-let)
    ("int" "(interactive)" t)
    ("cond" "" jacob-insert-lisp-cond)
    ("gc" "" jacob-insert-elisp-goto-char)
    ("pmi" "(point-min)" t)
    ("pma" "(point-max)" t)
    ("weal" "" jacob-insert-elisp-with-eval-after-load)
    ("mes" "" jacob-insert-elisp-message)))

(define-abbrev-table 'clojure-mode-abbrev-table
  '(("defn" "" jacob-insert-clojure-defn)
    ("if" "" jacob-insert-clojure-if)
    ("loop" "" jacob-insert-clojure-loop)
    ("rec" "" jacob-insert-clojure-recur)
    ("let" "" jacob-insert-clojure-let)
    ("case" "" jacob-insert-clojure-case)))

(define-abbrev-table 'go-mode-abbrev-table
  '(
    ("pl" "" jacob-insert-go-println)
    ("pf" "" jacob-insert-go-printf)
    ("fun" "" jacob-insert-go-func)
    ("for" "" jacob-insert-go-for)
    ("forr" "" jacob-insert-go-for-range)
    ("if" "" jacob-insert-go-if)
    ("struct" "" jacob-insert-go-struct)
    ("ass" ":=")
    )
  nil
  :parents (list common-operators-abbrev-table))

(define-abbrev-table 'purescript-mode-abbrev-table
  '(
    ("fa" "∀")
    ("ar" "->")
    ("nil" "Nil")
    ("maybe" "Maybe")
    ("unit" "Unit")
    ("int" "Int")
    ("boolean" "Boolean")
    ("nothing" "Nothing")
    ("just" "Just")
    ("effect" "Effect")
    ("list" "List")
    ("tuple" "Tuple")
    ))

(define-abbrev-table 'kotlin-mode-abbrev-table
  '(
    ("ar" "->")
    ("int" "Int")
    ("string" "String")
    ("char" "Char")
    ("list" "List")
    ("neq" "!=")
    ("fun" "" jacob-kotlin-function)
    ("val" "" jacob-kotlin-val)
    ("pl" "" jacob-kotlin-println)
    ("when" "" jacob-kotlin-when)
    ("listof" "" jacob-kotlin-list)
    ("test" "" jacob-kotlin-test)))

(define-abbrev-table 'sml-mode-abbrev-table
  '(
    ("val" "" jacob-sml-skeleton-val)
    ("if" "" jacob-sml-skeleton-if)
    ("let" "" jacob-sml-skeleton-let)
    ("fun" "" jacob-sml-skeleton-function)
    ("fn" "" jacob-sml-skeleton-anonymous-function)
    ("case" "" jacob-sml-skeleton-case)
    ))

(define-abbrev-table 'sql-mode-abbrev-table
  '(
    ("s" "SELECT")
    ("sa" "SELECT *")
    ("saf" "SELECT * FROM")
    ("f" "FROM")
    ("w" "WHERE")
    ("a" "AND")
    ("l" "LIKE")
    ("i" "IN")
    ("c" "CASE")
    ("wt" "WHEN THEN")
    ("lim" "LIMIT")
    ("j" "JOIN")
    ("o" "ON")
    ))

(define-jacob-insert jacob-racket-define-function
  (jacob-insert-helper "(define (■)\n●)"))

(define-abbrev-table 'racket-mode-abbrev-table
  '(
    ("deff" "" jacob-racket-define-function)
    ))



;; key bindings

(global-set-key (kbd "C-<next>") 'next-buffer)
(global-set-key (kbd "C-<prior>") 'previous-buffer)


;; macros

(fset 'jacob-return-macro [return])


;; xah-fly-keys keybindings

(jacob-is-installed 'xah-fly-keys

  (define-key global-map (kbd "M-SPC") 'xah-fly-command-mode-activate)

  (define-key xah-fly-command-map "s" 'jacob-return-macro)
  ;; (define-key xah-fly-command-map "4" 'other-window-prefix)
  (define-key xah-fly-command-map "1" 'winner-undo)
  (define-key xah-fly-command-map "2" 'winner-redo)
  (define-key xah-fly-command-map "9" 'jacob-swap-visible-buffers)
  (define-key xah-fly-command-map "'" 'jacob-format-words)
  (define-key xah-fly-insert-map (kbd "M-SPC") 'xah-fly-command-mode-activate)
  (jacob-is-installed 'expand-region
    (define-key xah-fly-command-map "8" 'er/expand-region))

  (jacob-is-installed 'switch-window
    (define-key xah-fly-command-map "," 'switch-window))

  (define-key xah-fly-command-map (kbd "=") 'jacob-next-error-or-punct)
  (define-key xah-fly-command-map (kbd "-") 'jacob-previous-error-or-punct)

  (define-prefix-command 'jacob-config-keymap)

  (define-key xah-fly-leader-key-map "u" 'kill-current-buffer)

  (define-prefix-command 'jacob-eglot-keymap)
  (define-key xah-fly-leader-key-map "we" jacob-eglot-keymap)
  (define-key xah-fly-leader-key-map "wea" 'eglot-code-actions)
  (define-key xah-fly-leader-key-map "wer" 'eglot-rename)

  (let ((map vc-prefix-map))
    (define-key map "p" 'vc-push)
    (define-key map "P" 'jacob-git-push-set-upstream)
    (define-key map "c" 'jacob-git-pull-master-new-branch))

  (let ((map project-prefix-map))
    (define-key map "g" 'jacob-project-search))

  (define-key xah-fly-leader-key-map ",n" 'jacob-eval-and-replace)
  (define-key xah-fly-leader-key-map "d," 'xah-insert-low-line)
  (define-key xah-fly-leader-key-map "d." 'jacob-insert-equals)
  (define-key xah-fly-leader-key-map "d/" 'jacob-insert-plus)
  (define-key xah-fly-leader-key-map "dc" 'jacob-insert-hash)
  (define-key xah-fly-leader-key-map "dd" 'backward-delete-char)
  (define-key xah-fly-leader-key-map "de" 'jacob-insert-dollar-sign)
  (define-key xah-fly-leader-key-map "dm" 'xah-insert-hyphen)
  (define-key xah-fly-leader-key-map "do" 'jacob-insert-ampersand)
  (define-key xah-fly-leader-key-map "dr" 'jacob-insert-caret)
  (define-key xah-fly-leader-key-map "dv" 'jacob-insert-tilde)
  (define-key xah-fly-leader-key-map "dx" 'jacob-insert-at)
  (define-key xah-fly-leader-key-map "dz" 'jacob-insert-apostrophe)
  
  (define-key xah-fly-leader-key-map "l8" 'ef-themes-toggle)
  ;; (define-key xah-fly-leader-key-map "ect" 'jacob-display-time) ; silly
  ;; (define-key xah-fly-leader-key-map "ei" 'jacob-format-buffer) ; should improve
  (define-key xah-fly-leader-key-map "ep" project-prefix-map)

  (define-key xah-fly-leader-key-map "l3" 'jacob-async-shell-command)
  (define-key xah-fly-leader-key-map "la" 'global-text-scale-adjust)

  (define-key xah-fly-leader-key-map "/c" 'vc-create-branch)
  (define-key xah-fly-leader-key-map "/b" 'vc-switch-branch)
  (define-key xah-fly-leader-key-map "/x" 'jacob-git-pull-master-new-branch)

  (jacob-is-installed 'consult
    (define-key xah-fly-leader-key-map "v" 'consult-yank-from-kill-ring)
    (define-key xah-fly-leader-key-map "f" 'consult-buffer)
    (define-key xah-fly-leader-key-map "ij" 'consult-recent-file)
    (define-key xah-fly-leader-key-map "es" 'consult-line))

  (defvar jacob-insert-parentheses-character ?k)
  (defvar jacob-insert-square-bracket-character ?l)
  (defvar jacob-insert-curly-brace-character ?j)
  (defvar jacob-insert-double-quote-character ?u)
  (defvar jacob-insert-single-quote-character ?i)
  (defvar jacob-insert-angle-bracket-character ?h)

  (setq insert-pair-alist `((,jacob-insert-parentheses-character ?\( ?\))
                            (,jacob-insert-square-bracket-character ?\[ ?\])
                            (,jacob-insert-curly-brace-character ?\{ ?\})
                            (,jacob-insert-double-quote-character ?\" ?\")
                            (,jacob-insert-single-quote-character ?\' ?\')
                            (,jacob-insert-angle-bracket-character ?\< ?\>)))

  (define-key xah-fly-leader-key-map (concat "d" (char-to-string jacob-insert-parentheses-character)) 'insert-pair)
  (define-key xah-fly-leader-key-map (concat "d" (char-to-string jacob-insert-square-bracket-character)) 'insert-pair)
  (define-key xah-fly-leader-key-map (concat "d" (char-to-string jacob-insert-curly-brace-character)) 'insert-pair)
  (define-key xah-fly-leader-key-map (concat "d" (char-to-string jacob-insert-double-quote-character)) 'insert-pair)
  (define-key xah-fly-leader-key-map (concat "d" (char-to-string jacob-insert-single-quote-character)) 'insert-pair)
  (define-key xah-fly-leader-key-map (concat "d" (char-to-string jacob-insert-angle-bracket-character)) 'insert-pair)

  (defvar jacob-recenter-repeat-map
    (let ((map (make-sparse-keymap)))
      (define-key map "p" 'recenter-top-bottom)
      map))

  (put 'recenter-top-bottom 'repeat-map 'jacob-recenter-repeat-map)

  (let ((map minibuffer-local-completion-map))
    (define-key map "SPC" 'self-insert-command))

  (let ((map dired-mode-map))
    (jacob-xfk-define-key-in-major-mode map "q" 'quit-window)
    (jacob-xfk-define-key-in-major-mode map "i" 'dired-previous-line)
    (jacob-xfk-define-key-in-major-mode map "k" 'dired-next-line)
    (jacob-xfk-define-key-in-major-mode map "s" 'dired-find-file)
    (jacob-xfk-define-key-in-major-mode map "e" 'dired-mark)
    (jacob-xfk-define-key-in-major-mode map "r" 'dired-unmark)
    (jacob-xfk-define-key-in-major-mode map "x" 'dired-do-rename)
    (jacob-xfk-define-key-in-major-mode map "c" 'dired-do-copy)
    (jacob-xfk-define-key-in-major-mode map "d" 'dired-do-delete) ; we skip the "flag, delete" process as files are sent to system bin on deletion
    (jacob-xfk-define-key-in-major-mode map "u" 'dired-up-directory)
    (jacob-xfk-define-key-in-major-mode map "j" 'dired-goto-file))

  (let ((map occur-mode-map))
    (jacob-xfk-define-key-in-major-mode map "q" 'quit-window)
    (jacob-xfk-define-key-in-major-mode map "i" 'previous-error-no-select)
    (jacob-xfk-define-key-in-major-mode map "k" 'next-error-no-select))

  (with-eval-after-load 'vc-dir
    (let ((map vc-dir-mode-map))
      (jacob-xfk-define-key-in-major-mode map "q" 'quit-window)
      (jacob-xfk-define-key-in-major-mode map "i" 'vc-dir-previous-line)
      (jacob-xfk-define-key-in-major-mode map "k" 'vc-dir-next-line)
      (jacob-xfk-define-key-in-major-mode map "o" 'vc-dir-next-directory)
      (jacob-xfk-define-key-in-major-mode map "u" 'vc-dir-previous-directory)
      (jacob-xfk-define-key-in-major-mode map "s" 'vc-dir-find-file)
      (jacob-xfk-define-key-in-major-mode map "e" 'vc-dir-mark)
      (jacob-xfk-define-key-in-major-mode map "r" 'vc-dir-unmark)
      (jacob-xfk-define-key-in-major-mode map "v" 'vc-next-action)
      (jacob-xfk-define-key-in-major-mode map "p" 'vc-push)
      (jacob-xfk-define-key-in-major-mode map "P" 'jacob-git-push-set-upstream)))

  (with-eval-after-load 'info
    (let ((map Info-mode-map))
      (jacob-xfk-define-key-in-major-mode map "q" 'quit-window)
      (jacob-xfk-define-key-in-major-mode map "r" 'Info-scroll-up)
      (jacob-xfk-define-key-in-major-mode map "e" 'Info-scroll-down)
      (jacob-xfk-define-key-in-major-mode map "w" 'Info-up)
      (jacob-xfk-define-key-in-major-mode map "g" 'Info-menu)))

  (with-eval-after-load 'calendar
    (let ((map calendar-mode-map))
      (jacob-xfk-define-key-in-major-mode map "q" 'quit-window)
      (jacob-xfk-define-key-in-major-mode map "i" 'calendar-backward-week)
      (jacob-xfk-define-key-in-major-mode map "k" 'calendar-forward-week)
      (jacob-xfk-define-key-in-major-mode map "j" 'calendar-backward-day)
      (jacob-xfk-define-key-in-major-mode map "l" 'calendar-forward-day)
      (jacob-xfk-define-key-in-major-mode map "u" 'calendar-backward-month)
      (jacob-xfk-define-key-in-major-mode map "o" 'calendar-forward-month)
      (jacob-xfk-define-key-in-major-mode map "d" 'diary-view-entries)
      (jacob-xfk-define-key-in-major-mode map "s" 'diary-insert-entry)
      (jacob-xfk-define-key-in-major-mode map "m" 'diary-mark-entries)
      (jacob-xfk-define-key-in-major-mode map "." 'calendar-goto-today)
      (jacob-xfk-define-key-in-major-mode map "t" 'calendar-set-mark)))

  (with-eval-after-load 'doc-view
    (let ((map doc-view-mode-map))
      (jacob-xfk-define-key-in-major-mode map "l" 'doc-view-next-page)
      (jacob-xfk-define-key-in-major-mode map "j" 'doc-view-previous-page)))

  (with-eval-after-load 'diff-mode
    (let ((map diff-mode-map))
      (jacob-xfk-define-key-in-major-mode map "q" 'quit-window))))

(with-eval-after-load 'smerge-mode
  (defvar jacob-smerge-repeat-map
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "n") 'smerge-next)
      (define-key map (kbd "p") 'smerge-prev)
      (define-key map (kbd "u") 'smerge-keep-upper)
      (define-key map (kbd "l") 'smerge-keep-lower)
      map))

  (put 'smerge-next 'repeat-map 'jacob-smerge-repeat-map)
  (put 'smerge-prev 'repeat-map 'jacob-smerge-repeat-map)
  (put 'smerge-keep-upper 'repeat-map 'jacob-smerge-repeat-map)
  (put 'smerge-keep-lower 'repeat-map 'jacob-smerge-repeat-map))

(define-key emacs-lisp-mode-map (kbd "C-j") 'jacob-eval-print-last-sexp)
(define-key lisp-interaction-mode-map (kbd "C-j") 'jacob-eval-print-last-sexp)

(provide 'init)
;;; init.el ends here
