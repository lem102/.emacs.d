;;; init.el --- Jacob's main init file. -*-lexical-binding: t; eval: (flymake-mode 0)-*-
;;; Commentary:
;;; Code:

;; built-in

;; use package

(eval-when-compile
  (require 'use-package))

(setopt use-package-always-ensure t
        use-package-hook-name-suffix nil
        use-package-enable-imenu-support t)



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


;; read environment file and variable setup

(defvar jacob-font-size 12
  "Font size to use.")

(defconst jacob-is-windows (eq system-type 'windows-nt)
  "Is the current OS windows?")

(defconst jacob-is-linux (eq system-type 'gnu/linux)
  "Is the current OS linux?")

(defvar jacob-stumpwm-installed (stringp (executable-find "stumpwm"))
  "Is stumpwm installed on the current system?")

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
(setq completion-ignore-case t)

(setq kill-buffer-query-functions (delq 'process-kill-buffer-query-function
                                        kill-buffer-query-functions))

(setq display-buffer-alist '(
                             ;; slack
                             ((or (derived-mode . slack-mode)
                                  (derived-mode . lui-mode))
                              (display-buffer-in-side-window)
                              (side . right))
                             ;; sql
                             ((major-mode . sql-interactive-mode)
                              (display-buffer-reuse-mode-window display-buffer-same-window))
                             ;; shell
                             ;; ("eshell\\*"
                             ;;  (display-buffer-in-side-window)
                             ;;  (side . bottom))
                             ))



(use-package desktop
  :config
  (desktop-save-mode 1))


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

(defvar jacob-welcome-messages '("\"A journey of a thousand miles begins with a single step.\" - 老子"
                                 "\"apex predator of grug is complexity\" - some grug"
                                 "\"Perfection is achieved, not when there is nothing more to add, but when there is nothing left to take away.\" - Antoine de Saint-Exupéry"
                                 "\"Always listen to Jiaqi.\" - Jacob Leeming"
                                 "\"The king wisely had the computer scientist beheaded, and they all lived happily ever after.\" - anon")
  "List of messages to display in scratch buffer.")

(setq initial-scratch-message (format ";; %s\n\n"
                                      (seq-random-elt jacob-welcome-messages)))

(setq parens-require-spaces nil)

(setq delete-pair-blink-delay 0)

;; support for files like `/etc/fstab'
(require 'generic-x)

(setq frame-resize-pixelwise t)
(setq save-interprogram-paste-before-kill t)


;; bookmark config

(with-eval-after-load 'bookmark
  (setopt bookmark-set-fringe-mark nil)
  (bookmark-store "emacs init file" '((filename . "~/.emacs.d/init.el")) nil)
  (bookmark-store "stumpwm init file" '((filename . "~/.stumpwm.d/init.lisp")) nil)
  (bookmark-store "emacs environment file" '((filename . "~/.emacs.d/environment.el")) nil))


;; unicode

(prefer-coding-system 'utf-8)


;; help at point

(setq-default help-at-pt-display-when-idle '(flymake-diagnostic))
(help-at-pt-set-timer)


;; help config
(setq help-window-select t)
(setq help-enable-variable-value-editing t)

(defun jacob-help-edit ()
  "Edit variable in current help buffer."
  (interactive)
  (unless (equal major-mode 'help-mode)
    (message "not in help buffer"))
  (save-excursion
    (goto-char (point-min))
    (if (search-forward "Its value is " nil "NOERROR")
        (help-fns-edit-variable)
      (message "cannot find editable variable"))))


;; warnings config

(setq warning-minimum-level :error)


;; editing config

(global-subword-mode 1)

(setq show-paren-when-point-inside-paren t)
(show-paren-mode 1)

(electric-pair-mode 1)

(delete-selection-mode 1)

(repeat-mode 1)

(setq dabbrev-case-fold-search nil)
(setq dabbrev-case-replace nil)


;; modeline config

(column-number-mode 0)
(line-number-mode 1)

(setq mode-line-percent-position nil)



(use-package vc
  :defer t
  :custom
  (vc-git-show-stash 0 "show 0 stashes")
  ;; (vc-ignore-dir-regexp
  ;;  (format "\\(%s\\)\\|\\(%s\\)"
  ;;          vc-ignore-dir-regexp
  ;;          tramp-file-name-regexp)
  ;;  "ignore tramp files")
  )



(use-package autoinsert
  :config
  (auto-insert-mode t)
  :custom
  (auto-insert-query nil)
  (auto-insert-directory (locate-user-emacs-file "templates")))



(use-package tramp
  :defer t
  :config
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path)
  :custom
  (tramp-archive-enabled nil "lots of problems. for now, disable it!"))


;; theme config

(load-theme 'modus-operandi "NO-CONFIRM")


;; js-mode config

(put 'js-indent-level 'safe-local-variable #'numberp)
(setq js-indent-level 2)


;; cc-mode config

(setq-default c-basic-offset 4)



(use-package csharp-mode
  :defer t
  :config
  ;; JACOBTODO: include only my modifications rather than the whole data structure
  (setopt csharp-ts-mode--indent-rules
          `((c-sharp
             ((parent-is "compilation_unit") parent-bol 0)
             ((node-is "}") parent-bol 0)
             ((node-is ")") parent-bol 0)
             ((node-is "]") parent-bol 0)
             ((and (parent-is "comment") c-ts-common-looking-at-star)
              c-ts-common-comment-start-after-first-star -1)
             ((parent-is "comment") prev-adaptive-prefix 0)
             ((parent-is "namespace_declaration") parent-bol 0)
             ((parent-is "class_declaration") parent-bol 0)
             ((parent-is "constructor_declaration") parent-bol 0)
             ((parent-is "initializer_expression") parent-bol csharp-ts-mode-indent-offset)
             ((match "{" "anonymous_object_creation_expression") parent-bol 0)
             ((parent-is "anonymous_object_creation_expression") parent-bol csharp-ts-mode-indent-offset)
             ((match "{" "object_creation_expression") parent-bol 0)
             ((parent-is "object_creation_expression") parent-bol 0)
             ((parent-is "method_declaration") parent-bol 0)
             ((parent-is "enum_declaration") parent-bol 0)
             ((parent-is "operator_declaration") parent-bol 0)
             ((parent-is "field_declaration") parent-bol 0)
             ((parent-is "struct_declaration") parent-bol 0)
             ((parent-is "declaration_list") parent-bol csharp-ts-mode-indent-offset)
             ((parent-is "argument_list") parent-bol csharp-ts-mode-indent-offset)
             ((parent-is "interpolation") parent-bol csharp-ts-mode-indent-offset)
             ((parent-is "binary_expression") parent 0)
             ((parent-is "block") parent-bol csharp-ts-mode-indent-offset)
             ((parent-is "local_function_statement") parent-bol 0)
             ((match "block" "if_statement") parent-bol 0)
             ((match "else" "if_statement") parent-bol 0)
             ((parent-is "if_statement") parent-bol csharp-ts-mode-indent-offset)
             ((parent-is "for_statement") parent-bol 0)
             ((parent-is "for_each_statement") parent-bol 0)
             ((parent-is "while_statement") parent-bol 0)
             ((match "{" "switch_expression") parent-bol 0)
             ((parent-is "switch_statement") parent-bol 0)
             ((parent-is "switch_body") parent-bol csharp-ts-mode-indent-offset)
             ((parent-is "switch_section") parent-bol csharp-ts-mode-indent-offset)
             ((parent-is "switch_expression") parent-bol csharp-ts-mode-indent-offset)
             ((parent-is "case_statement") parent-bol 0)
             ((parent-is "do_statement") parent-bol 0)
             ((parent-is "equals_value_clause") parent-bol csharp-ts-mode-indent-offset)
             ((parent-is "ternary_expression") parent-bol csharp-ts-mode-indent-offset)
             ((parent-is "conditional_expression") parent-bol csharp-ts-mode-indent-offset)
             ((parent-is "statement_block") parent-bol csharp-ts-mode-indent-offset)
             ((parent-is "type_arguments") parent-bol csharp-ts-mode-indent-offset)
             ((parent-is "variable_declarator") parent-bol csharp-ts-mode-indent-offset)
             ((parent-is "arguments") parent-bol csharp-ts-mode-indent-offset)
             ((parent-is "array") parent-bol csharp-ts-mode-indent-offset)
             ((parent-is "formal_parameters") parent-bol csharp-ts-mode-indent-offset)
             ((parent-is "template_substitution") parent-bol csharp-ts-mode-indent-offset)
             ((parent-is "object_pattern") parent-bol csharp-ts-mode-indent-offset)
             ((parent-is "object") parent-bol csharp-ts-mode-indent-offset)
             ((parent-is "object_type") parent-bol csharp-ts-mode-indent-offset)
             ((parent-is "enum_body") parent-bol csharp-ts-mode-indent-offset)
             ((parent-is "arrow_function") parent-bol csharp-ts-mode-indent-offset)
             ((parent-is "parenthesized_expression") parent-bol csharp-ts-mode-indent-offset)
             ;; what i have added
             ((parent-is "parameter_list") parent-bol csharp-ts-mode-indent-offset)
             ((parent-is "implicit_parameter_list") parent-bol csharp-ts-mode-indent-offset)
             ((parent-is "member_access_expression") parent-bol csharp-ts-mode-indent-offset)
             ((parent-is "lambda_expression") parent-bol csharp-ts-mode-indent-offset)
             ((parent-is "try_statement") parent-bol 0)
             ((parent-is "catch_clause") parent-bol 0)
             ((parent-is "record_declaration") parent-bol 0)
             ((parent-is "interface_declaration") parent-bol 0)
             ((parent-is "throw_expression") parent-bol csharp-ts-mode-indent-offset)
             ((parent-is "return_statement") parent-bol csharp-ts-mode-indent-offset)
             ((parent-is "record_declaration") parent-bol 0)
             ((parent-is "interface_declaration") parent-bol 0)
             )))

  ;; JACOBTODO: merge into emacs core
  (nconc csharp-ts-mode--font-lock-settings
         (treesit-font-lock-rules
          :language 'c-sharp
          :feature 'property
          :override t
          `((property_declaration
             type: (generic_name name: (identifier) @font-lock-type-face)
             name: (identifier) @font-lock-variable-name-face))))
  
  (defun jacob-csharp-ts-hook-function ()
    "Set vars in csharp-ts-mode buffer."
    (setq treesit-defun-type-regexp "\\(method\\|constructor\\|field\\)_declaration")
    (setq jacob-backspace-function #'jacob-backspace-csharp))

  (add-hook 'csharp-ts-mode-hook 'jacob-csharp-ts-hook-function)

  (add-to-list 'compilation-error-regexp-alist-alist
               '(jacob-dotnet-stacktrace-re
                 "   at [^
]+ in \\(.+\\):line \\([[:digit:]]+\\)"
                 1
                 2))

  (add-to-list 'compilation-error-regexp-alist 'jacob-dotnet-stacktrace-re)

  (defun jacob-insert-class ()
    "Insert a csharp class, for use in abbrev."
    (let ((start (point)))
      (insert (format "class %s\n{\n\n}"
                      (file-name-base (buffer-file-name))))
      (indent-region start (point))
      (forward-line -1)
      (indent-for-tab-command))
    t)

  (put 'jacob-insert-class 'no-self-insert t))

(defun jacob-csharp-forward-statement ()
  "Move forward over a csharp statement."
  (interactive)
  (treesit-end-of-thing "statement"))

(defun jacob-csharp-backward-statement ()
  "Move backward over a csharp statement."
  (interactive)
  (treesit-beginning-of-thing "statement"))

(defun jacob-csharp-beginning-of-line-or-statement ()
  "Move cursor to the beginning of line or previous csharp statement."
  (interactive)
  (let ((p (point)))
    (if (eq last-command this-command)
        (call-interactively 'jacob-csharp-backward-statement)
      (back-to-indentation)
      (when (eq p (point))
        (beginning-of-line)))))

(defun jacob-csharp-end-of-line-or-statement ()
  "Move cursor to the end of line or next csharp statement."
  (interactive)
  (let ((p (point)))
    (if (eq last-command this-command)
        (call-interactively 'jacob-csharp-forward-statement)
      (end-of-line))))


;; common lisp config

(setq inferior-lisp-program "sbcl")


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

  (setq dired-vc-rename-file t)

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
  ;; (setq flymake-mode-line-format '(flymake-mode-line-exception flymake-mode-line-counters))
  )


;; project config

(with-eval-after-load 'project
  (setq project-switch-commands '((project-find-file "Find file")
                                  (jacob-project-search "Find regexp")
                                  (project-find-dir "Find directory")
                                  (project-vc-dir "VC-Dir")
                                  (project-eshell "Eshell")
                                  (project-compile "Compile"))))

(with-eval-after-load 'consult

  (defun jacob-consult-project-filter ()
    (if (project-current)
        (project-files (project-current))
      (list)))

  (defvar jacob-consult-project-source
    `(:name     "Project"
                :narrow   ?p
                :category file
                :face     consult-file
                :history  file-name-history
                :items    ,#'jacob-consult-project-filter
                :action   ,#'find-file))

  (unless jacob-is-windows
    (add-to-list 'consult-buffer-sources jacob-consult-project-source "APPEND")))


;; emacs-lisp-mode config

(defun jacob-elisp-config-hook-function ()
  "Configure `emacs-lisp-mode' when hook run."
  (flymake-mode 1)
  (eldoc-mode 1)
  (setq-local parens-require-spaces t))

(add-hook 'emacs-lisp-mode-hook #'jacob-elisp-config-hook-function)


;; font config

(set-frame-font (format "%s-%s"
                        (cdr (assoc-string system-type
                                           '(("windows-nt" . "Consolas")
                                             ("darwin" . "Menlo")
                                             ("gnu/linux" . "DejaVu Sans Mono"))))
                        jacob-font-size)
                "KEEP-SIZE"
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



(use-package org-contrib
  :config
  (require 'ox-extra)
  (ox-extras-activate '(latex-header-blocks ignore-headlines)))


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



(use-package smerge-mode
  :defer t
  :bind
  (:repeat-map jacob-smerge-repeat-map
               ("l" . #'smerge-next)
               ("j" . #'smerge-prev)
               ("i" . #'smerge-keep-upper)
               ("k" . #'smerge-keep-lower)
               ("SPC" . #'smerge-keep-all)))


;; time emacs startup

(add-hook 'emacs-startup-hook
          (lambda ()
            (message (emacs-init-time (concat "Emacs ready in %.2f seconds "
                                              (format "with %d garbage collections"
                                                      gcs-done))))))



(use-package calendar
  :defer t
  :config
  (add-hook 'calendar-today-visible-hook 'calendar-mark-today)
  :custom
  (diary-date-forms diary-european-date-forms)
  (calendar-date-style 'european)
  (calendar-date-display-form '((if dayname
                                    (concat dayname ", "))
                                day "/" month "/" year))
  (calendar-week-start-day 1)
  (calendar-mark-diary-entries-flag t)
  (calendar-mark-holidays-flag t))


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

(when jacob-is-windows
  (defun jacob-confirm-terminate-batch-job ()
    "Type y and enter to terminate batch job after sending ^C."
    (when (not (null eshell-process-list))
      (insert "y")
      (eshell-send-input)))

  (advice-add 'eshell-interrupt-process :after #'jacob-confirm-terminate-batch-job)

  (setopt find-program "C:/Program Files (x86)/GnuWin32/bin/find.exe"))



(use-package winner
  :after xah-fly-keys
  :demand
  :config
  (winner-mode 1)
  :bind ( :map xah-fly-command-map
          ("1" . winner-undo)
          ("2" . winner-redo)))



(use-package sql
  :commands jacob-sql-connect
  :config
  (defun jacob-sql-connect (connection &optional buf-name)
    "Wrapper for sql connect to set postgres password."
    (interactive
     (if sql-connection-alist
         (list (progn
                 (require 'sql)
                 (sql-read-connection "Connection: "))
               current-prefix-arg)
       (user-error "No SQL Connections defined")))
    (with-environment-variables
        (("PGPASSWORD" (cadr (assoc 'sql-password
                                    (assoc-string connection
                                                  sql-connection-alist
                                                  t)))))
      (sql-connect connection buf-name)))
  (defun jacob-sql-interactive-mode-hook ()
    "Custom interactive SQL mode behaviours.

See `sql-interactive-mode-hook' and `sql-product-alist'."
    (when (eq sql-product 'postgres)
      (setq sql-prompt-regexp "^[-[:alnum:]_]*[-=]\\*?[#>] ")
      (setq sql-prompt-cont-regexp "^\\(?:\\sw\\|\\s_\\)*[-(]\\*?[#>] ")))

  (defun jacob-sqli-end-of-buffer ()
    "Move point to end of sqli buffer before sending paragraph.

Intended as before advice for `sql-send-paragraph'."
    (with-current-buffer sql-buffer
      (end-of-buffer)))

  (advice-add #'sql-send-paragraph :before #'jacob-sqli-end-of-buffer)

  (jacob-xfk-define-key-in-major-mode sql-mode-map " ,d" #'sql-send-paragraph)
  :hook (sql-interactive-mode-hook . jacob-sql-interactive-mode-hook))


;; docview config

(with-eval-after-load 'doc-view-mode

  (defun jacob-doc-view-hook ()
    "hook function for doc view mode"
    (auto-revert-mode 1))

  (add-hook 'doc-view-mode-hook 'jacob-doc-view-hook)

  (when jacob-is-windows
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

(add-hook 'compilation-filter-hook 'ansi-color-compilation-filter)


;; treesit config

;; strategy for adopting tree-sitter:
;; on linux, use the auto build stuff included in emacs
;; on windows, grab the .dlls from a bundle

(when jacob-is-linux
  (setq treesit-language-source-alist '((c-sharp "https://github.com/tree-sitter/tree-sitter-c-sharp" "master" "src")
                                        (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
                                        (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
                                        (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
                                        (yaml "https://github.com/ikatyang/tree-sitter-yaml")))
  (setq treesit-load-name-override-list '((c-sharp "libtree-sitter-csharp" "tree_sitter_c_sharp"))))

(setq major-mode-remap-alist '((csharp-mode . csharp-ts-mode)
                               (javascript-mode . js-ts-mode)))
(setq treesit-font-lock-level 4)      ; max level of fontification


;; eglot config

;; JACOBTODO: function that can smartly decide between jumping to
;; definition or implementation (`xref-find-definitions' vs
;; `eglot-find-implementation')

;; WIP
(defun jacob-go-definition ()
  "If not in an eglot buffer, do regular xref stuff.

Otherwise, go to implementation. If already at implementation go
to definition."
  (interactive)
  (if (not eglot--managed-mode)
      (call-interactively #'xref-find-definitions)
    (let ((start-buffer (current-buffer)))
      (ignore-errors
        (eglot-find-implementation))
      (when (eq start-buffer (current-buffer))
        ;; JACOBTODO: won't work, this just takes us to the current
        ;; method. if language server implemented go to declaration
        ;; something might be possible.
        (call-interactively #'xref-find-definitions)))))

(setq eglot-ignored-server-capabilities '(:documentOnTypeFormattingProvider))

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
(add-hook 'csharp-ts-mode-hook 'eglot-ensure)

(add-hook 'fsharp-mode-hook (lambda ()
                              (when jacob-is-linux
                                (require 'eglot-fsharp)
                                (eglot-ensure))))
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs `((csharp-mode csharp-ts-mode) . ,(eglot-alternatives
                                                                         '(("csharp-ls")
                                                                           ("OmniSharp" "-lsp")
                                                                           ("Microsoft.CodeAnalysis.LanguageServer"
                                                                            "--logLevel=Information"
                                                                            "--extensionLogDirectory=/home/jacobl/dev/roslynLSP/logs/")))))

  (add-to-list 'eglot-server-programs '(sql-mode . ("sqls")))

  (add-to-list 'eglot-server-programs `((js-mode
                                         js-ts-mode
                                         tsx-ts-mode
                                         (typescript-ts-base-mode :language-id "typescript")
                                         typescript-mode)
                                        . ,(eglot-alternatives
                                            '(("typescript-language-server" "--stdio")
                                              ("deno" "lsp"
                                               :initializationOptions
                                               (:enable t :lint t :suggest.names t))))))

  (eglot--code-action eglot-code-action-organize-imports-ts "source.organizeImports.ts")
  (eglot--code-action eglot-code-action-add-missing-imports-ts "source.addMissingImports.ts")

  (defun eglot--format-markup (markup)
    "Format MARKUP according to LSP's spec."
    (pcase-let ((`(,string ,mode)
                 (if (stringp markup) (list markup 'gfm-view-mode)
                   (list (plist-get markup :value)
                         (pcase (plist-get markup :kind)
                           ;; changed this line, before was gfm-view-mode instead of markdown-view-mode
                           ("markdown" 'markdown-view-mode)
                           ("plaintext" 'text-mode)
                           (_ major-mode))))))
      (with-temp-buffer
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
  (setq gnus-use-full-window t)
  (setq gnus-always-read-dribble-file t)
  (add-hook 'gnus-after-getting-new-news-hook 'gnus-notifications)

  (defun jacob-gnus-hook-function ()
    "Hook function to be used with a gnus hook."
    (gnus-demon-add-handler 'gnus-demon-scan-news 2 t))

  (add-hook 'gnus-started-hook 'jacob-gnus-hook-function)

  (add-hook 'gnus-group-mode-hook 'gnus-topic-mode))



;; package configuration

;; for most packages installing from latest is fine. for special
;; cases, use package-vc

(require 'package)

(setopt package-archives '(("GNU" . "https://elpa.gnu.org/packages/")
                           ("non-GNU" . "https://elpa.nongnu.org/nongnu/")
                           ("melpa" . "https://melpa.org/packages/")))


;; use-package-vc

;; JACOBTODO: built in as of emacs v30. usage may also change

(unless (package-installed-p 'vc-use-package)
  (package-vc-install "https://github.com/slotThe/vc-use-package"))
(require 'vc-use-package)



(use-package key-chord
  :vc (key-chord :url "https://github.com/emacsorphanage/key-chord.git")
  :config
  (key-chord-mode 1))



(use-package use-package-chords)



(use-package avy
  :after use-package-chords
  :chords (("fj" . avy-goto-char-timer))
  :config
  (defun jacob-avy-action-xref (pt)
    (save-excursion
      (goto-char pt)
      (call-interactively #'xref-find-definitions))
    (select-window
     (cdr (ring-ref avy-ring 0)))
    t)
  :custom
  (avy-keys '(?a ?s ?d ?f ?g ?h ?j ?l ?\;))
  (avy-dispatch-alist '((?y . avy-action-yank)
                        (?k . avy-action-kill-stay)
                        (?K . avy-action-kill-move)
                        (?t . avy-action-teleport)
                        (?m . avy-action-mark)
                        (?w . avy-action-copy)
                        (?i . avy-action-ispell)
                        (?z . avy-action-zap-to-char)
                        (?. . jacob-avy-action-xref))))



(use-package apheleia
  :config
  (apheleia-global-mode 1)
  (setq-default apheleia-inhibit t)     ; set `apheleia-inhibit' to
                                        ; nil to enable
  (push '(csharpier "dotnet" "csharpier" "--write-stdout")
        apheleia-formatters))


;; combobulate config
(unless (package-installed-p 'combobulate)
  (package-vc-install "https://github.com/lem102/combobulate.git"))

(with-eval-after-load 'combobulate
  (setq combobulate-key-prefix "C-o o"))

(add-hook 'typescript-ts-mode-hook #'combobulate-mode)


;; eglot-booster config

(unless jacob-is-windows
  (with-eval-after-load 'eglot
    (unless (package-installed-p 'eglot-booster)
      (package-vc-install "https://github.com/jdtsmith/eglot-booster"))

    (eglot-booster-mode 1)))


;; slack config

(unless (package-installed-p 'slack)
  (package-vc-install '(slack . (:url "https://github.com/lem102/emacs-slack.git"))))

(defun jacob-slack-modeline-formatter (alist)
  "Hide the slack modeline if there are no notifications.

Element in ALIST is  '((team-name . ((thread . (has-unreads . mention-count)) (channel . (has-unreads . mention-count)))))"
  (if (seq-find (lambda (team)
                  (seq-find (lambda (room-type)
                              (car (cdr room-type)))
                            (cdr team)))
                alist)
      (slack-default-modeline-formatter alist)
    ""))

(defun jacob-slack-show-unread ()
  "Open an unread slack message."
  (interactive)
  (let* ((team (slack-team-select))
         (rooms (seq-filter #'(lambda (room)
                                (slack-room-has-unread-p room team))
                            (append (slack-team-ims team)
                                    (slack-team-groups team)
                                    (slack-team-channels team)))))
    (if (null rooms)
        (message "no unread slack messages")
      (slack-room-display (seq-first rooms) team))))

(defun jacob-slack-kill-buffers ()
  "Kill all slack message buffers."
  (interactive)
  (kill-some-buffers
   (seq-filter (lambda (b)
                 (seq-contains-p
                  '(slack-message-buffer-mode
                    slack-thread-message-buffer-mode
                    slack-file-info-buffer-mode)
                  (buffer-local-value 'major-mode b)))
               (buffer-list))))

(defun jacob-slack-show-all-unread ()
  "Show all unread messages."
  (interactive)
  (let* ((team (slack-team-select))
         (rooms (seq-filter #'(lambda (room)
                                (slack-room-has-unread-p room team))
                            (append (slack-team-ims team)
                                    (slack-team-groups team)
                                    (slack-team-channels team)))))
    (if (null rooms)
        (message "no unread slack messages")
      (dolist (room rooms)
        (slack-room-display room team)))))

(with-eval-after-load 'slack
  (setq slack-enable-global-mode-string t)
  (setq slack-buffer-emojify t)
  (setq slack-prefer-current-team t)
  (setq slack-thread-also-send-to-room nil)

  (setq alert-default-style 'notifications)

  (setq lui-fill-type nil)
  (setq lui-time-stamp-position 0)
  (setq lui-time-stamp-format "%a %b %e %H:%M")

  (defun jacob-slack-hook-function ()
    "Function to be run in slack mode hooks."
    (toggle-word-wrap 1))

  (add-hook 'slack-message-buffer-mode-hook 'jacob-slack-hook-function)
  (add-hook 'slack-thread-message-buffer-mode-hook 'jacob-slack-hook-function)

  (setq slack-modeline-formatter #'jacob-slack-modeline-formatter)

  (with-eval-after-load 'consult

    (defun jacob-consult-slack-filter ()
      (consult--buffer-query :sort 'visibility
                             :as #'buffer-name
                             :include "^*Slack"))

    (setq jacob-consult-slack-source
          `(:name     "Slack"
                      :narrow   ?s
                      :category buffer
                      :face     consult-buffer
                      :history  buffer-name-history
                      :items    ,#'jacob-consult-slack-filter
                      :action   ,#'switch-to-buffer))

    (add-to-list 'consult-buffer-sources jacob-consult-slack-source "APPEND")))



(use-package csharp-toolbox
  :after csharp-mode
  :vc (csharp-toolbox :url "https://github.com/lem102/csharp-toolbox.git"))



;; JACOBTODO: jacob-csharp package requires dape, causing it to be loaded prematurely
(use-package dape
  :commands dape
  :custom
  (dape-info-hide-mode-line nil)
  (dape-buffer-window-arrangment 'right)
  :config

  (push '(netcoredbg-attach-port
          modes (csharp-mode csharp-ts-mode)
          ensure dape-ensure-command
          command "netcoredbg"
          command-args ["--interpreter=vscode"]
          :request "attach"
          :cwd dape-cwd-fn
          :program jacob-select-dll
          :stopAtEntry t
          :processId
          (lambda ()
            (let* ((collection
                    (seq-map
                     (lambda (pid)
                       (cons (cdr (assoc 'args
                                         (process-attributes pid)))
                             pid))
                     (list-system-processes)))
                   (selection (completing-read "process: "
                                               collection)))
              (cdr (assoc selection collection)))))
        dape-configs))



(use-package switch-window
  ;; JACOBTODO: investigate switch window finish hook to solve compilation scroll issue
  :after xah-fly-keys
  :custom
  (switch-window-shortcut-style 'qwerty)
  (switch-window-threshold 3)
  :bind ( :map xah-fly-command-map
          ("," . switch-window)))



(use-package racket-mode
  :hook (racket-mode-hook . racket-xp-mode))


;; auctex config (weird)

(use-package tex
  :ensure auctex
  :mode ("\\.tex\\$" . latex-mode)
  :config
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq-default japanese-TeX-error-messages nil)
  (TeX-global-PDF-mode 0))



(use-package ob-restclient)



(use-package fsharp-mode
  :defer t
  :custom
  ;; (inferior-fsharp-program "dotnet fsi --fsi-server-input-codepage:65001")
  (inferior-fsharp-program "dotnet fsi"))

(use-package eglot-fsharp
  :after fsharp-mode
  :custom
  (eglot-fsharp-server-install-dir nil))



(use-package purescript-mode
  :defer t
  :hook (purescript-mode-hook . turn-on-purescript-indentation))



(use-package orderless
  :after vertico
  :custom
  (completion-styles '(orderless initials)))



(use-package vertico
  :custom
  (vertico-count 25)
  :config
  (vertico-mode 1))



(use-package marginalia
  :after vertico
  :config
  (marginalia-mode 1))



(use-package consult
  :after xah-fly-keys
  :custom
  (completion-in-region-function 'consult-completion-in-region)
  (xref-show-xrefs-function 'consult-xref)
  (xref-show-definitions-function 'consult-xref)
  (consult--source-buffer (plist-put consult--source-buffer
                                     :state #'jacob-consult-buffer-state-no-tramp))
  :config
  (defun jacob-project-search ()
    "If current project is a git project, use consult git grep, otherwise use consult grep."
    (interactive)
    (if (vc-find-root default-directory ".git")
        (consult-git-grep)
      (consult-grep)))
  
  (defun jacob-consult-buffer-state-no-tramp ()
    "Buffer state function that doesn't preview Tramp buffers."
    (let ((orig-state (consult--buffer-state))
          (filter (lambda (action candidate)
                    (if (and candidate
                             (or (eq action 'return)
                                 (let ((buffer (get-buffer candidate)))
                                   (and buffer
                                        (not (file-remote-p (buffer-local-value 'default-directory buffer)))))))
                        candidate
                      nil))))
      (lambda (action candidate)
        (funcall orig-state action (funcall filter action cand)))))
  :bind ( :map xah-fly-leader-key-map
          ("v" . consult-yank-from-kill-ring)
          ("f" . consult-buffer)
          ("ij" . consult-recent-file)
          ("es" . consult-line)
          ("ku" . consult-goto-line)))



(use-package expand-region
  :after xah-fly-keys
  :custom
  (expand-region-contract-fast-key "9")
  :bind ( :map xah-fly-command-map
          ("8" . er/expand-region)))



(use-package sml-mode
  :custom
  (sml-abbrev-skeletons nil))



(use-package xah-fly-keys
  :custom
  (xah-fly-use-control-key nil)
  (xah-fly-use-meta-key nil)
  :config
  (xah-fly-keys-set-layout "qwerty")
  (xah-fly-keys 1)

  ;; FIXME: keys that are not already bound will not work for jacob-xfk-define-key-in-major-mode
  (defun jacob-xfk-define-key-in-major-mode (major-mode-keymap key command)
    "In MAJOR-MODE-KEYMAP bind KEY to COMMAND only when in xfk command mode."
    (define-key major-mode-keymap
                (vector 'remap
                        (lookup-key xah-fly-command-map key))
                command)))



(use-package verb
  :hook (org-mode-hook . verb-mode))



(use-package web-mode
  :mode "\\.cshtml\\'")



(use-package sly
  :after xah-fly-keys
  :config
  (sly-symbol-completion-mode 0)

  (jacob-xfk-define-key-in-major-mode lisp-mode-map " ,m" #'sly-eval-last-expression)
  (jacob-xfk-define-key-in-major-mode lisp-mode-map " ,d" #'sly-eval-defun)
  (jacob-xfk-define-key-in-major-mode lisp-mode-map " ,e" #'sly-eval-buffer)
  (jacob-xfk-define-key-in-major-mode lisp-mode-map " wk" #'sly-edit-definition))



(use-package yasnippet
  :config
  (yas-global-mode 1)
  (define-key yas-minor-mode-map (kbd "SPC") yas-maybe-expand)

  (defun jacob-snippet-mode-hook ()
    "Function to be run in hook for `snippet-mode'."
    ;; hopefully fix weird issues which can happen in snippet-mode
    ;; buffers
    (setopt auto-save-visited-mode nil))

  (add-hook 'snippet-mode-hook 'jacob-snippet-mode-hook)

  (defun jacob-autoinsert-yas-expand ()
    "Replace text in yasnippet template."
    (yas-expand-snippet (buffer-string) (point-min) (point-max)))

  (define-auto-insert "\\.cs$" [ "template.cs" jacob-autoinsert-yas-expand ]))



;; personal functions

(defun jacob-ip-to-kill-ring ()
  "Copy v4 ip address to kill ring."
  (interactive)
  (kill-new (with-temp-buffer (shell-command "curl --silent -4 ifconfig.me"
                                             t)
                              (buffer-string))))

(defun jacob-beginning-of-line ()
  "Go to indentation, line start, backward paragraph."
  (interactive)
  (cond ((bolp)
         (backward-paragraph))
        ((= (save-excursion
              (back-to-indentation)
              (point))
            (point))
         (move-beginning-of-line 1))
        (t
         (back-to-indentation))))

(defun jacob-end-of-line ()
  "Go to content end, line end, forward paragraph."
  (interactive)
  (if (eolp)
      (forward-paragraph)
    (let ((content-end (save-excursion
                         (when (and (derived-mode-p 'prog-mode)
                                    (comment-search-forward (line-end-position) "NOERROR"))
                           (skip-syntax-backward "< " (line-beginning-position))
                           (unless (= (point) (line-beginning-position))
                             (point))))))
      (if (or (null content-end)
              (= content-end (point)))
          (move-end-of-line 1)
        (goto-char content-end)))))

(defun jacob-kill-line ()
  "If region is active, kill it. Otherwise:

If point is at the beginning of the line, kill the whole line.

If point is at the end of the line, kill until the beginning of the line.

Otherwise, kill from point to the end of the line."
  (interactive)
  (cond ((region-active-p)
         (call-interactively #'kill-region))
        ((bolp)
         (kill-whole-line))
        ((eolp)
         (kill-line 0))
        (t
         (kill-line))))

(defvar-local jacob-backspace-function nil
  "Called by `jacob-backspace' if non-nil.")

(defun jacob-backspace ()
  "DWIM backspace command.

If character to the left is a pair character as determined by
`insert-pair-alist', kill from the pair to its match. If the
prefix argument is provided, just delete the pair characters."
  (interactive)
  (undo-boundary)
  (if (region-active-p)
      (delete-active-region)
    (when (= 1 (point))
      (user-error "Beginning of buffer"))
    (let ((char-class (char-syntax (char-before)))
          (f (if current-prefix-arg
                 #'delete-pair
               #'kill-sexp)))
      (unless (ignore-errors
                (funcall jacob-backspace-function f))
        (cond ((= ?\" char-class)                         ; string
               (if (nth 3 (syntax-ppss))
                   (backward-char)
                 (backward-sexp))
               (funcall f))
              ((= ?\( char-class)                         ; delete from start of pair
               (backward-char)
               (funcall f))
              ((= ?\) char-class)                         ; delete from end of pair
               (backward-sexp)
               (funcall f))
              (t                                          ; delete character
               (backward-delete-char 1)))))))

(defun jacob-backspace-csharp (f)
  "Function for `jacob-backspace' to help with csharp.

Figure out if the `<' or `>' before point is part of a
`type_argument_list', and delete accordingly. F is used to
perform the deletion."
  (when (or (= (char-before) ?<)
            (= (char-before) ?>))
    (let ((node-parent (save-excursion
                         (backward-char)
                         (treesit-node-type
                          (treesit-node-parent
                           (treesit-node-at (point)))))))
      (when (string= node-parent "type_argument_list")
        (let ((table (copy-syntax-table csharp-mode-syntax-table)))
          (modify-syntax-entry ?< "(>" table)
          (modify-syntax-entry ?> ")>" table)
          (with-syntax-table table
            (if (= (char-before) ?<)
                (backward-char)
              (backward-sexp))
            (funcall f)))
        t))))

(defun jacob-random-init ()
  "Go to a random place in init file."
  (interactive)
  (find-file user-init-file)
  (goto-char (random (point-max))))

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

(defun jacob-goto-pi ()
  "Connect to raspberry pi."
  (interactive)
  (find-file jacob-raspberry-pi-connection-string))

(defun jacob-toggle-modeline ()
  "Toggle visibility of modeline."
  (interactive)
  (set 'mode-line-format (eval
                          (car
                           (get 'mode-line-format
                                'standard-value)))))

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
                       (format "* %s %s *" default-directory command)))

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
    (setq jacob-format-words-style-and-start (cons (read-char-from-minibuffer "select style: " '(?c ?p ?k ?s ?S))
                                                   (point))))

  (save-excursion
    (let* ((style (car jacob-format-words-style-and-start))
           (format-position (cdr jacob-format-words-style-and-start))
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

      (setq jacob-format-words-style-and-start (cons style (point))))))

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

(defun jacob-git-push-set-upstream ()
  "Push current git branch to new upstream branch."
  (interactive)
  (shell-command "git push --set-upstream origin HEAD"))

(defvar jacob-git-lab-push-set-upstream-jira-url ""
  "URL for current employer's jira board.")

(defun jacob-git-lab-push-set-upstream ()
  "Push the current branch and create an upstream branch.
Use GitLab push options to create a merge request and set all
necessary values.

For use with GitLab only."
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
                                          "\" ")))))
    (with-temp-buffer
      (eshell-command command t))))

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

(defun jacob-gitlab-link-at-point ()
  "Create a gitlab link for the line of code at point."
  (interactive)
  (let* ((project (project-current))
         (repository (project-name project))
         (branch "development")
         (filepath (file-relative-name buffer-file-name (project-root project)))
         (line-number (line-number-at-pos))
         (link (format "https://gitlab.com/tappit-leeds-devs/%s/-/blob/%s/%s#L%d"
                       repository
                       branch
                       filepath
                       line-number)))
    (kill-new link)
    (message "%s" link)))



;; abbrevs


;; abbrev config

(setq abbrev-suggest t)
(set-default 'abbrev-mode t)
(setq save-abbrevs nil)


;; jacob-insert-config

;; JACOBTODO: this is cool, but jank. look into https://github.com/minad/tempel

(defun jacob-insert (&optional template)
  "Handle `jacob-insert' abbrev expansion.
Insert TEMPLATE.  If present, move point back to ■.  ■ will be
deleted."
  (when template
    (insert template))
  (let* ((end-position (point))
         (start-position (if template
                             (- end-position
                                (length template))
                           last-abbrev-location))
         ■-position)
    (when (search-backward-regexp "■"
                                  start-position
                                  t)
      (delete-char 1)
      (setq ■-position (point-marker)))
    (indent-region start-position end-position)
    (when ■-position
      (goto-char ■-position))))

(put 'jacob-insert 'no-self-insert t)

(defun jacob-point-in-text-p ()
  "Return t if in comment or string. Else nil."
  (let ((xsyntax-state (syntax-ppss)))
    (or (nth 3 xsyntax-state)
        (nth 4 xsyntax-state))))

(defun jacob-point-in-code-p ()
  "Return t if outside of string or comment. Else nil."
  (not (jacob-point-in-text-p)))

(define-abbrev-table 'text-mode-abbrev-table
  '(("i" "I")
    ("im" "I'm")
    ("idd" "I'd")
    ("dont" "don't")
    ("its" "it's")
    ("havent" "haven't")))

(define-abbrev-table 'common-c-abbrev-table
  '(("lt" "<")
    ("gt" ">")
    ("lte" "<=")
    ("gte" ">=")
    ("eq" "==")
    ("neq" "!=")
    ("or" "||")
    ("and" "&&")
    ("ret" "return"))
  :enable-function 'jacob-point-in-code-p)

(define-abbrev-table 'jacob-comment-abbrev-table
  '(("jt" "JACOBTODO:"))
  nil
  :enable-function 'jacob-point-in-text-p)

(define-abbrev-table 'js-ts-mode-abbrev-table
  '(("cl" "console.log(■);" jacob-insert)
    ("fun" "(■) => " jacob-insert)
    ("con" "const ■ = " jacob-insert)
    ("let" "let ■ = " jacob-insert)
    ("fore" "forEach((■) => )" jacob-insert)
    ("map" "map((■) => )" jacob-insert)
    ("filter" "filter((■) => )" jacob-insert)
    ("red" "reduce((■) => )" jacob-insert)
    ("jwe" "console.log(\"jacobwozere\");" t)
    ("eeq" "===")
    ("neeq" "!==")
    ("if" "if (■) {\n\n}" jacob-insert)
    ("for" "for (■) {\n\n}" jacob-insert)
    ("while" "while (■) {\n}" jacob-insert)
    ("switch" "switch (■) {\n}" jacob-insert)
    ("case" "case ■: \n\nbreak;" jacob-insert))
  nil
  :parents (list jacob-comment-abbrev-table)
  :enable-function 'jacob-point-in-code-p)

(define-abbrev-table 'typescript-ts-mode-abbrev-table
  nil
  nil
  :parents (list js-ts-mode-abbrev-table))

(define-abbrev-table 'tsx-ts-mode-abbrev-table
  nil
  nil
  :parents (list js-ts-mode-abbrev-table))

(define-abbrev-table 'csharp-ts-mode-abbrev-table
  '(("class" "" jacob-insert-class)
    ("cons" "public ■ ()\n{\n\n}" jacob-insert)
    ("var" "var ■ = " jacob-insert)
    ("meth" "void ■()\n{\n\n}" jacob-insert)
    ("switche" "switch\n{\n■\n}" jacob-insert)
    ("cl" "Console.WriteLine(■);" jacob-insert)
    ("prop" "public ■ { get; set; }" jacob-insert)
    ("field" "private ■ _" jacob-insert)
    ("jwe" "Console.WriteLine(\"jacobwozere\");" t)
    ;; JACOBTODO: cant insert tostring inside interpolated string thing e.g. $"bla bla {variable.tostr}" won't work
    ("tostr" "ToString()" t)
    ("iia" "It.IsAny<■>()" jacob-insert)
    ("az" "async")
    ("ns" "namespace")
    ("xgon" "x => x")
    ("ro" "readonly")
    ("nuguid" "Guid.NewGuid()")
    ("pri" "private")
    ("pub" "public")
    ("sta" "static")
    ("ret" "return")
    ("eq" "==")
    ("fun" "(■) => " jacob-insert)
    ("if" "if (■)\n{\n\n}" jacob-insert)
    ("for" "for (■)\n{\n\n}" jacob-insert)
    ("while" "while (■)\n{\n}" jacob-insert)
    ("switch" "switch (■)\n{\n}" jacob-insert)
    ("case" "case ■: \n\nbreak;" jacob-insert)
    ("band" "&&")
    ("bor" "||"))
  nil
  :parents (list jacob-comment-abbrev-table)
  :enable-function 'jacob-point-in-code-p)

(define-abbrev-table 'emacs-lisp-mode-abbrev-table
  '(("up" "use-package" t)
    ("d" "defun" t)
    ("ah" "add-hook" t)
    ("l" "lambda" t)
    ("let" "(let ((■))\n)" jacob-insert)
    ("cond" "(cond ((■))\n)" jacob-insert)
    ("gc" "(goto-char ■)" jacob-insert)
    ("weal" "(with-eval-after-load ■)" jacob-insert)
    ("mes" "(message \"%s\" ■)" jacob-insert)
    ("if" "(if ■)" jacob-insert)
    ("pmi" "(point-min)")
    ("pma" "(point-max)")
    ("int" "(interactive)"))
  nil
  :parents (list jacob-comment-abbrev-table)
  :enable-function 'jacob-point-in-code-p)

(define-abbrev-table 'lisp-mode-abbrev-table
  '()
  nil
  :parents (list jacob-comment-abbrev-table)
  :enable-function 'jacob-point-in-code-p)

(define-abbrev-table 'go-mode-abbrev-table
  '(("pl" "fmt.Println(■)" jacob-insert)
    ("pf" "fmt.Printf(■)" jacob-insert)
    ("fun" "func ■()\n{\n\n}" jacob-insert)
    ("for" "for ■\n{\n\n}" jacob-insert)
    ("forr" "for i, v := range ■\n{\n\n}" jacob-insert)
    ("if" "if ■\n{\n\n}" jacob-insert)
    ("struct" "struct\n{\n\n}" jacob-insert)
    ("ass" ":="))
  nil
  :parents (list common-c-abbrev-table))

(define-abbrev-table 'purescript-mode-abbrev-table
  '(("fa" "∀")
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
    ("tuple" "Tuple")))

(define-abbrev-table 'sql-mode-abbrev-table
  '(("sel" "SELECT *\nFROM ■\nWHERE condition;" jacob-insert)
    ("upd" "UPDATE ■\nSET x = y\nWHERE condition;" jacob-insert)
    ("del" "DELETE FROM ■\nWHERE condition;" jacob-insert)
    ("joi" "JOIN ■\nON field = field" jacob-insert)
    ("ins" "INSERT INTO ■ (column, column2)\nVALUES (value, value2)" jacob-insert)
    ("ord" "ORDER BY")
    ("gro" "GROUP BY")
    ("and" "AND")
    ("as" "AS")))



;; keybindings

;; default keybinds that can be rebound:

(keymap-set lisp-interaction-mode-map "C-j" #'jacob-eval-print-last-sexp)

(keymap-global-unset "C-x C-c")         ; `save-buffers-kill-terminal'
(keymap-global-unset "C-z")             ; `suspend-frame'
(keymap-global-unset "C-x u")           ; `undo'

(with-eval-after-load 'consult
  (keymap-set project-prefix-map "g" #'jacob-project-search)
  (keymap-global-set "M-g i" #'consult-imenu))


;; macros

(fset 'jacob-return-macro [return])


;; xah-fly-keys keybindings

(with-eval-after-load 'xah-fly-keys

  (global-set-key (kbd "<f7>") 'xah-fly-leader-key-map)

  (define-key global-map (kbd "M-SPC") 'xah-fly-command-mode-activate)

  (define-key xah-fly-command-map "h" #'jacob-beginning-of-line)
  (define-key xah-fly-command-map ";" #'jacob-end-of-line)
  (define-key xah-fly-command-map "s" #'jacob-return-macro)
  (define-key xah-fly-command-map "d" #'jacob-backspace)
  ;; (define-key xah-fly-command-map "4" 'other-window-prefix)
  (define-key xah-fly-command-map "9" 'jacob-swap-visible-buffers)
  (define-key xah-fly-command-map "'" 'jacob-format-words)
  (define-key xah-fly-insert-map (kbd "M-SPC") 'xah-fly-command-mode-activate)

  (define-key xah-fly-command-map (kbd "=") 'jacob-next-error-or-punct)
  (define-key xah-fly-command-map (kbd "-") 'jacob-previous-error-or-punct)

  (define-key xah-fly-leader-key-map "u" 'kill-current-buffer)

  ;; (define-key map "P" 'jacob-git-push-set-upstream)

  (let ((map project-prefix-map))
    (define-key map "g" 'jacob-project-search))

  (define-key xah-fly-leader-key-map ",n" 'jacob-eval-and-replace)

  (define-key xah-fly-leader-key-map "ep" project-prefix-map)

  (define-key xah-fly-leader-key-map "l3" 'jacob-async-shell-command)
  (define-key xah-fly-leader-key-map "l8" 'modus-themes-toggle)
  (define-key xah-fly-leader-key-map "la" 'global-text-scale-adjust)

  (define-key xah-fly-leader-key-map "/c" 'vc-create-branch)
  (define-key xah-fly-leader-key-map "/b" 'vc-switch-branch)
  (define-key xah-fly-leader-key-map "/x" 'jacob-git-pull-master-new-branch)

  (define-key xah-fly-leader-key-map "wj" 'xref-find-references)

  (setq insert-pair-alist (append insert-pair-alist
                                  '((?k ?\( ?\))
                                    (?l ?\[ ?\])
                                    (?j ?\{ ?\})
                                    (?u ?\" ?\")
                                    (?i ?\' ?\')
                                    (?h ?\< ?\>))))

  (define-key xah-fly-leader-key-map "dk" 'insert-pair)
  (define-key xah-fly-leader-key-map "dl" 'insert-pair)
  (define-key xah-fly-leader-key-map "dj" 'insert-pair)
  (define-key xah-fly-leader-key-map "du" 'insert-pair)
  (define-key xah-fly-leader-key-map "di" 'insert-pair)
  (define-key xah-fly-leader-key-map "dh" 'insert-pair)

  (defvar jacob-recenter-repeat-map
    (let ((map (make-sparse-keymap)))
      (define-key map "p" 'recenter-top-bottom)
      map))

  (put 'recenter-top-bottom 'repeat-map 'jacob-recenter-repeat-map)

  (let ((map minibuffer-local-completion-map))
    (define-key map "SPC" 'self-insert-command))

  (keymap-unset xah-fly-leader-key-map "SPC")

  (keymap-set xah-fly-leader-key-map "SPC e e" #'eglot)

  (with-eval-after-load 'eglot
    (keymap-set xah-fly-leader-key-map "SPC e a" #'eglot-code-actions)
    (keymap-set xah-fly-leader-key-map "SPC e r" #'eglot-rename)
    (keymap-set xah-fly-leader-key-map "SPC e i" #'eglot-find-implementation)
    (keymap-set xah-fly-leader-key-map "SPC e R" #'eglot-reconnect))

  (when (package-installed-p 'slack)
    (keymap-set xah-fly-leader-key-map "SPC s s" #'slack-start))

  (with-eval-after-load 'slack
    (keymap-set xah-fly-leader-key-map "SPC s u" #'jacob-slack-show-unread)
    (keymap-set xah-fly-leader-key-map "SPC s U" #'jacob-slack-show-all-unread)
    (keymap-set xah-fly-leader-key-map "SPC s r" #'slack-select-rooms)
    (keymap-set xah-fly-leader-key-map "SPC s k" #'jacob-slack-kill-buffers))

  (defvar-keymap jacob-csharp-map
    "f" #'jacob-format-csharp-statement
    "t" #'jacob-csharp-run-test)

  (keymap-set xah-fly-leader-key-map "SPC c f" #'jacob-format-csharp-statement)
  (keymap-set xah-fly-leader-key-map "SPC c t" #'jacob-csharp-run-test)

  (keymap-set xah-fly-leader-key-map "SPC g" #'gnus)
  (keymap-set xah-fly-leader-key-map "SPC d" #'jacob-sql-connect)

  (let ((map dired-mode-map))
    (jacob-xfk-define-key-in-major-mode map "q" 'quit-window)
    (jacob-xfk-define-key-in-major-mode map "i" 'dired-previous-line)
    (jacob-xfk-define-key-in-major-mode map "k" 'dired-next-line)
    (jacob-xfk-define-key-in-major-mode map "s" 'dired-find-file)
    (jacob-xfk-define-key-in-major-mode map "e" 'dired-mark)
    (jacob-xfk-define-key-in-major-mode map "r" 'dired-unmark)
    (jacob-xfk-define-key-in-major-mode map "g" #'revert-buffer)
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
    (jacob-xfk-define-key-in-major-mode vc-dir-mode-map "q" 'quit-window)
    (jacob-xfk-define-key-in-major-mode vc-dir-mode-map "g" 'revert-buffer)
    (jacob-xfk-define-key-in-major-mode vc-dir-mode-map "i" 'vc-dir-previous-line)
    (jacob-xfk-define-key-in-major-mode vc-dir-mode-map "k" 'vc-dir-next-line)
    (jacob-xfk-define-key-in-major-mode vc-dir-mode-map "o" 'vc-dir-next-directory)
    (jacob-xfk-define-key-in-major-mode vc-dir-mode-map "u" 'vc-dir-previous-directory)
    (jacob-xfk-define-key-in-major-mode vc-dir-mode-map "s" 'vc-dir-find-file)
    (jacob-xfk-define-key-in-major-mode vc-dir-mode-map "e" 'vc-dir-mark)
    (jacob-xfk-define-key-in-major-mode vc-dir-mode-map "r" 'vc-dir-unmark)
    (jacob-xfk-define-key-in-major-mode vc-dir-mode-map "v" 'vc-next-action)
    (jacob-xfk-define-key-in-major-mode vc-dir-mode-map "p" 'vc-push)
    (jacob-xfk-define-key-in-major-mode vc-dir-mode-map "P" 'jacob-git-push-set-upstream)
    (jacob-xfk-define-key-in-major-mode vc-dir-mode-map "=" 'vc-diff)
    (jacob-xfk-define-key-in-major-mode vc-dir-mode-map "x" #'vc-dir-hide-up-to-date))

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
      (jacob-xfk-define-key-in-major-mode map "q" #'quit-window)
      (jacob-xfk-define-key-in-major-mode map "e" #'diff-hunk-prev)
      (jacob-xfk-define-key-in-major-mode map "r" #'diff-hunk-next)
      (jacob-xfk-define-key-in-major-mode map "x" #'diff-hunk-kill)
      (jacob-xfk-define-key-in-major-mode map "g" #'revert-buffer)))

  (with-eval-after-load 'vc-annotate
    (let ((map vc-annotate-mode-map))
      (jacob-xfk-define-key-in-major-mode map "q" #'quit-window)
      (jacob-xfk-define-key-in-major-mode map "g" #'revert-buffer)))

  (with-eval-after-load 'vc-git
    (jacob-xfk-define-key-in-major-mode vc-git-log-view-mode-map "q" #'quit-window))

  (with-eval-after-load 'gnus
    (jacob-xfk-define-key-in-major-mode gnus-group-mode-map "q" #'gnus-group-exit)
    (jacob-xfk-define-key-in-major-mode gnus-group-mode-map "i" #'gnus-group-prev-group)
    (jacob-xfk-define-key-in-major-mode gnus-group-mode-map "k" #'gnus-group-next-group)
    (jacob-xfk-define-key-in-major-mode gnus-group-mode-map "g" #'gnus-group-get-new-news)

    (with-eval-after-load 'gnus-topic
      (jacob-xfk-define-key-in-major-mode gnus-topic-mode-map "s" #'gnus-topic-select-group))

    (jacob-xfk-define-key-in-major-mode gnus-summary-mode-map "q" #'gnus-summary-exit)
    (jacob-xfk-define-key-in-major-mode gnus-summary-mode-map "i" #'gnus-summary-prev-article)
    (jacob-xfk-define-key-in-major-mode gnus-summary-mode-map "k" #'gnus-summary-next-article)
    (jacob-xfk-define-key-in-major-mode gnus-summary-mode-map "j" #'gnus-summary-prev-page)
    (jacob-xfk-define-key-in-major-mode gnus-summary-mode-map "l" #'gnus-summary-next-page))

  (with-eval-after-load 'help
    (jacob-xfk-define-key-in-major-mode help-mode-map "q" #'quit-window)
    (jacob-xfk-define-key-in-major-mode help-mode-map "e" #'help-go-back)
    (jacob-xfk-define-key-in-major-mode help-mode-map "r" #'jacob-go-forward)
    (jacob-xfk-define-key-in-major-mode help-mode-map "w" #'jacob-help-edit)
    (jacob-xfk-define-key-in-major-mode help-mode-map "g" #'revert-buffer)
    (jacob-xfk-define-key-in-major-mode help-mode-map "s" #'help-view-source))

  (with-eval-after-load 'compile
    (jacob-xfk-define-key-in-major-mode compilation-mode-map "g" #'recompile))

  (with-eval-after-load 'csharp-mode
    (let ((map csharp-ts-mode-map))
      ;; (jacob-xfk-define-key-in-major-mode map "h" 'jacob-csharp-beginning-of-line-or-statement)
      ;; (jacob-xfk-define-key-in-major-mode map ";" 'jacob-csharp-end-of-line-or-statement)
      ))

  (with-eval-after-load 'js
    (jacob-xfk-define-key-in-major-mode js-ts-mode-map " ,c" #'recompile))

  (with-eval-after-load 'typescript-ts-mode
    (jacob-xfk-define-key-in-major-mode typescript-ts-mode-map " ,c" #'recompile)))

(provide 'init)
;;; init.el ends here
