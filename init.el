;;; init.el --- Jacob's main init file. -*-lexical-binding: t; eval: (flymake-mode 0)-*-
;;; Commentary:
;;; Code:

(setq-default truncate-lines nil)
(setq-default tab-width 4) ; set default tab char's display width to 4 spaces


;; read environment file and variable setup

(defvar jacob-font-size 11
  "Font size to use.")

(defconst jacob-is-windows (eq system-type 'windows-nt)
  "Is the current OS windows?")

(defconst jacob-is-linux (eq system-type 'gnu/linux)
  "Is the current OS linux?")

(when (file-exists-p "~/.emacs.d/environment.el")
  (load-file "~/.emacs.d/environment.el"))



(require 'package)

(defvar jacob-require-already-refreshed nil
  "If nil, haven't refreshed packages with `jacob-require' yet.")

(defun jacob-require (package &optional vc)
  "Ensure PACKAGE is installed, then `require' it.

If VC is provided, it is passed to `package-vc-install' to install the
package rather than using `package-install'."
  (if (or
       ;; these files do not `provide' a feature and therefore cannot be `require'd
       (member package '(startup lisp mule-cmds bindings indent))
       (featurep package)
       (require package nil "NOERROR"))
      package
    (if vc
        (unless (package-installed-p package)
          (package-vc-install vc))
      (unless (package-installed-p package)
        (unless jacob-require-already-refreshed
          (package-refresh-contents)
          (setq jacob-require-already-refreshed t))
        (package-install package)))
    (require package)))

(setopt package-archives '(("GNU" . "https://elpa.gnu.org/packages/")
                           ("non-GNU" . "https://elpa.nongnu.org/nongnu/")
                           ("melpa" . "https://melpa.org/packages/")))



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

(add-to-list 'default-frame-alist
             `(font . ,(format "%s-%s"
                               (cdr (assoc-string system-type
                                                  '(("windows-nt" . "Consolas")
                                                    ("darwin" . "Menlo")
                                                    ("gnu/linux" . "DejaVu Sans Mono"))))
                               jacob-font-size)))

(keymap-global-unset "C-x C-c")         ; `save-buffers-kill-terminal'
(keymap-global-unset "C-z")             ; `suspend-frame'
(keymap-global-unset "C-x u")           ; `undo'

(setopt delete-by-moving-to-trash t
        read-process-output-max (* 1024 1024)
        frame-resize-pixelwise t
        create-lockfiles nil
        history-length 1000
        history-delete-duplicates t
        scroll-conservatively 101
        use-dialog-box nil
        use-short-answers t
        ring-bell-function 'ignore
        truncate-partial-wdth-windows nil
        enable-recursive-minibuffers t
        completion-ignore-case t
        kill-buffer-query-functions (delq 'process-kill-buffer-query-function
                                          kill-buffer-query-functions))

;; built-in

(require 'use-package)

(setopt use-package-hook-name-suffix nil
        use-package-enable-imenu-support t)

(defmacro jacob-define-hook-function (hook &rest body)
  "Define function with BODY and bind it to HOOK."
  (declare (indent defun))
  (let* ((hook-name (symbol-name hook))
         (function-name (intern (concat "jacob-" hook-name "-function"))))
    `(progn
       (defun ,function-name ()
         ,(format "Auto-generated hook function for `%s'." hook-name)
         ,@body)
       (add-hook ',hook #',function-name))))

(require 'abbrev)

(add-hook 'text-mode-hook 'abbrev-mode)
(add-hook 'prog-mode-hook 'abbrev-mode)

(defun jacob-setup-abbrev-table (table abbrevs &rest properties)
  "Setup an abbrev table.
Clear the abbrev TABLE.  Then populate it with ABBREVS.  Finally,
set the PROPERTIES of TABLE."
  ;; JACOBTODO: use this function instead of `define-abbrev-table'.
  ;; JACOBTODO: overwrite parents? 🤔 I think yes, you could use
  ;; `abbrev-table-get' to get the original property and combine
  ;; them in arguments
  (clear-abbrev-table table)
  (dolist (abbrev abbrevs)
    (apply #'define-abbrev table abbrev))
  ;; stolen from abbrev.el itself
  (while (consp properties)
    (unless (cdr properties)
      (error "Missing value for property %S"
             (car properties)))
    (abbrev-table-put table
                      (pop properties)
                      (pop properties))))

(defun jacob-point-in-text-p ()
  "Return t if in comment or string. Else nil."
  (let ((xsyntax-state (syntax-ppss)))
    (or (nth 3 xsyntax-state)
        (nth 4 xsyntax-state))))

(defun jacob-point-in-code-p ()
  "Return t if outside of string or comment. Else nil."
  (not (jacob-point-in-text-p)))

(defun jacob-abbrev-no-insert ()
  "No-op function with `no-self-insert' property."
  t)
(put 'jacob-abbrev-no-insert 'no-self-insert t)

(define-abbrev-table 'jacob-comment-abbrev-table
  '(("jt" "JACOBTODO:"))
  nil
  :enable-function 'jacob-point-in-text-p)

(setopt abbrev-suggest t
        save-abbrevs nil)

(require 'mwheel)

(setopt mouse-wheel-progressive-speed nil
        mouse-wheel-scroll-amount '(10 ((shift) . hscroll)
                                       ((meta))
                                       ((control) . text-scale)))

(use-package files
  :config
  (auto-save-visited-mode 1)
  :custom
  (auto-save-default nil)
  (make-backup-files nil)
  (backup-by-copying t)
  (confirm-kill-processes nil)
  (auto-save-visited-interval 2 "save file after two seconds")
  (auto-save-visited-predicate (lambda () ; JACOBTODO: disable in hook, get rid of this
                                 (not (equal major-mode 'message-mode)))))

(use-package window
  :config
  ;; JACOBTODO: convert to newer syntax
  (defvar jacob-recenter-repeat-map
    (let ((map (make-sparse-keymap)))
      (define-key map "p" 'recenter-top-bottom)
      map))

  (put 'recenter-top-bottom 'repeat-map 'jacob-recenter-repeat-map)
  :custom
  (switch-to-buffer-obey-display-actions t)
  (display-buffer-alist '(
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
  (split-height-threshold nil))

(use-package frame
  :custom
  (blink-cursor-blinks 0 "make cursor blink forever"))

(use-package novice
  :defer
  :custom
  (disabled-command-function nil))

(use-package desktop
  :init
  (desktop-save-mode 1)
  :config
  (add-to-list 'desktop-minor-mode-table '(treesit-explore-mode nil))
  :custom
  (desktop-restore-eager 5)
  (desktop-lazy-verbose nil))

(use-package recentf
  :init
  (recentf-mode 1))

(use-package savehist
  :init
  (savehist-mode 1)
  :custom
  (savehist-save-minibuffer-history t))

(use-package saveplace
  :init
  (save-place-mode 1)
  :custom
  (save-place-forget-unreadable-files t))

(use-package cus-edit
  :custom
  (custom-file (make-temp-file "emacs-custom-")))

(jacob-require 'startup)
(setopt inhibit-startup-screen t
        initial-scratch-message (format ";; %s\n\n"
                                        (seq-random-elt
                                         '("\"A journey of a thousand miles begins with a single step.\" - 老子"
                                           "\"apex predator of grug is complexity\" - some grug"
                                           "\"Perfection is achieved, not when there is nothing more to add, but when there is nothing left to take away.\" - Antoine de Saint-Exupéry"
                                           "\"Always listen to Jiaqi.\" - Jacob Leeming"
                                           "\"The king wisely had the computer scientist beheaded, and they all lived happily ever after.\" - anon"
                                           "\"Success is going from failure to failure without losing your enthusiasm.\" - Winston Churchill (maybe)"))))

(jacob-require 'lisp)
(setopt parens-require-spaces nil
        delete-pair-blink-delay 0
        insert-pair-alist (append insert-pair-alist
                                  '((?k ?\( ?\))
                                    (?l ?\[ ?\])
                                    (?j ?\{ ?\})
                                    (?u ?\" ?\")
                                    (?i ?\' ?\')
                                    (?h ?\< ?\>))))

(jacob-require 'lisp-mode)
(jacob-setup-abbrev-table lisp-mode-abbrev-table
                          nil
                          :parents (list jacob-comment-abbrev-table)
                          :enable-function 'jacob-point-in-code-p)

;; support for files like `/etc/fstab'
(use-package generic-x)

(use-package simple
  :init
  (column-number-mode 1)
  (line-number-mode 1)
  (setq-default indent-tabs-mode nil)   ; use spaces to indent
  :custom
  (save-interprogram-paste-before-kill t))

(use-package bookmark
  :defer
  :config
  (bookmark-store "emacs init file" '((filename . "~/.emacs.d/init.el")) nil)
  (bookmark-store "emacs environment file" '((filename . "~/.emacs.d/environment.el")) nil)
  :custom
  (bookmark-set-fringe-mark nil))

(provide 'mule-cmds)                    ; HACK
(use-package mule-cmds
  :defer
  :config
  (prefer-coding-system 'utf-8))

(use-package help
  :defer
  :custom
  (help-window-select t)
  (help-enable-variable-value-editing t))

(use-package help-mode
  :after xah-fly-keys help-fns
  :config
  (jacob-define-hook-function help-mode-hook
    (jacob-xfk-local-key "s" #'help-view-source)
    (jacob-xfk-local-key "q" #'quit-window)
    (jacob-xfk-local-key "e" #'help-go-back)
    (jacob-xfk-local-key "r" #'help-go-forward)
    (jacob-xfk-local-key "g" #'revert-buffer)
    (jacob-xfk-local-key "w" #'jacob-help-edit)))

(use-package help-fns
  :after xah-fly-keys
  :config
  (defun jacob-help-edit ()
    "Edit variable in current help buffer."
    (interactive)
    (unless (equal major-mode 'help-mode)
      (message "not in help buffer"))
    (save-excursion
      (goto-char (point-min))
      (if (search-forward "Its value is " nil "NOERROR")
          (help-fns-edit-variable)
        (message "cannot find editable variable")))))

(use-package help-at-pt
  :defer
  :after flymake
  :config
  (setq-default help-at-pt-display-when-idle '(flymake-diagnostic))
  (help-at-pt-set-timer))

(use-package warnings
  :defer
  :custom
  (warning-minimum-level :error))

(use-package subword
  :config
  (global-subword-mode 1))

(use-package paren
  :config
  (show-paren-mode 1)
  :custom
  (show-paren-when-point-inside-paren t))

(use-package elec-pair
  :config
  (electric-pair-mode 1))

(use-package delsel
  :config
  (delete-selection-mode 1))

(use-package repeat
  :config
  (repeat-mode 1))

(use-package dabbrev
  :defer
  :custom
  (dabbrev-case-fold-search nil)
  (dabbrev-case-replace nil))

(provide 'bindings)                     ; HACK
(use-package bindings
  :custom
  (mode-line-percent-position nil))

(use-package vc
  :defer
  :custom
  (vc-git-show-stash 0 "show 0 stashes")
  (vc-ignore-dir-regexp
   (format "\\(%s\\)\\|\\(%s\\)"
           vc-ignore-dir-regexp
           tramp-file-name-regexp)
   "ignore tramp files"))

(jacob-require 'vc-git)
(jacob-define-hook-function vc-git-log-view-mode-hook
  (jacob-xfk-local-key "q" #'quit-window))

(jacob-require 'vc-dir)
(jacob-define-hook-function vc-dir-mode-hook
  (jacob-xfk-local-key "q" #'quit-window)
  (jacob-xfk-local-key "g" #'revert-buffer)
  (jacob-xfk-local-key "i" #'vc-dir-previous-line)
  (jacob-xfk-local-key "k" #'vc-dir-next-line)
  (jacob-xfk-local-key "o" #'vc-dir-next-directory)
  (jacob-xfk-local-key "u" #'vc-dir-previous-directory)
  (jacob-xfk-local-key "s" #'vc-dir-find-file)
  (jacob-xfk-local-key "e" #'vc-dir-mark)
  (jacob-xfk-local-key "r" #'vc-dir-unmark)
  (jacob-xfk-local-key "v" #'vc-next-action)
  (jacob-xfk-local-key "p" #'vc-push)
  (jacob-xfk-local-key "P" #'jacob-git-push-set-upstream)
  (jacob-xfk-local-key "=" #'vc-diff)
  (jacob-xfk-local-key "x" #'vc-dir-hide-up-to-date))

(use-package autoinsert
  :defer
  :config
  (auto-insert-mode t)
  :custom
  (auto-insert-query t)
  (auto-insert-directory (locate-user-emacs-file "templates")))

(use-package tramp
  :defer
  :config
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path)
  :custom
  (tramp-archive-enabled nil "lots of problems. for now, disable it!"))

(use-package csharp-mode
  :hook (csharp-ts-mode-hook . jacob-csharp-ts-hook-function)
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

  (add-to-list 'compilation-error-regexp-alist-alist
               '(jacob-dotnet-stacktrace-re
                 "   at [^
]+ in \\(.+\\):line \\([[:digit:]]+\\)"
                 1
                 2))

  (add-to-list 'compilation-error-regexp-alist 'jacob-dotnet-stacktrace-re)

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

  (jacob-setup-abbrev-table csharp-ts-mode-abbrev-table
                            ;; JACOBTODO: cant insert abbrevs inside interpolated strings
                            '(("v" "var" jacob-abbrev-no-insert)
                              ("tostr" "ToString()" jacob-abbrev-no-insert)
                              ("jwe" "Console.WriteLine(\"jacobwozere\");" jacob-abbrev-no-insert)
                              ("az" "async")
                              ("ns" "namespace")
                              ("xgon" "x => x")
                              ("ro" "readonly")
                              ("nuguid" "Guid.NewGuid()")
                              ("pri" "private")
                              ("pub" "public")
                              ("sta" "static")
                              ("req" "required")
                              ("gs" "{ get; set; }" jacob-abbrev-no-insert)
                              ("ret" "return")
                              ("eq" "==")
                              ("neq" "!=")
                              ("band" "&&")
                              ("bor" "||"))
                            :parents (list jacob-comment-abbrev-table)
                            :enable-function 'jacob-point-in-code-p))

(jacob-require 'inf-lisp)
(setopt inferior-lisp-program "sbcl")

(jacob-require 'dired)
(jacob-define-hook-function dired-mode-hook
  (dired-hide-details-mode 1)
  (jacob-xfk-local-key "s" #'dired-find-file)
  (jacob-xfk-local-key "d" #'dired-do-delete) ; we skip the "flag, delete" process as files are sent to system bin on deletion
  (jacob-xfk-local-key "q" #'quit-window)
  (jacob-xfk-local-key "i" #'dired-previous-line)
  (jacob-xfk-local-key "k" #'dired-next-line)
  (jacob-xfk-local-key "e" #'dired-mark)
  (jacob-xfk-local-key "r" #'dired-unmark)
  (jacob-xfk-local-key "g" #'revert-buffer)
  (jacob-xfk-local-key "x" #'dired-do-rename)
  (jacob-xfk-local-key "c" #'dired-do-copy)
  (jacob-xfk-local-key "u" #'dired-up-directory)
  (jacob-xfk-local-key "j" #'dired-goto-file))

(setopt dired-recursive-copies 'always
        dired-dwim-target t
        dired-listing-switches "-hal" ; the h option needs to come first 🙃
        dired-guess-shell-alist-user '(("\\.mkv\\'" "mpv")))

(jacob-require 'dired-aux)
(setopt dired-vc-rename-file t)

(jacob-require 'ls-lisp)
(setopt ls-lisp-use-insert-directory-program nil
        ls-lisp-dirs-first t)

(use-package esh-mode
  :defer
  :custom
  (eshell-scroll-to-bottom-on-output t)
  :config
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

(use-package esh-proc
  :after esh-mode
  :config
  (when jacob-is-windows
    (defun jacob-confirm-terminate-batch-job ()
      "Type y and enter to terminate batch job after sending ^C."
      (when (not (null eshell-process-list))
        (insert "y")
        (eshell-send-input)))
    
    (advice-add 'eshell-interrupt-process :after #'jacob-confirm-terminate-batch-job)))

(jacob-require 'eldoc)
(global-eldoc-mode 1)

(use-package project
  :defer
  :custom
  (project-switch-commands '((project-find-file "Find file")
                             (jacob-project-search "Find regexp")
                             (project-find-dir "Find directory")
                             (project-vc-dir "VC-Dir")
                             (project-eshell "Eshell")
                             (project-compile "Compile"))))

(use-package elisp-mode
  :init
  (defun jacob-elisp-config-hook-function ()
    "Configure `emacs-lisp-mode' when hook run."
    (flymake-mode 1)
    (add-hook 'before-save-hook 'jacob-indent-buffer nil "LOCAL")
    (setq-local yas-key-syntaxes '("w_")))
  :hook (emacs-lisp-mode-hook . jacob-elisp-config-hook-function)
  :config
  (add-to-list 'lisp-imenu-generic-expression '("Packages" "^(\\(jacob-\\)*require '\\([a-z-]+\\)" 2))
  (jacob-setup-abbrev-table emacs-lisp-mode-abbrev-table
                            '(("up" "use-package" jacob-abbrev-no-insert)
                              ("d" "defun" jacob-abbrev-no-insert)
                              ("p" "point" jacob-abbrev-no-insert)
                              ("point" "(point)" jacob-abbrev-no-insert)
                              ("ah" "add-hook" jacob-abbrev-no-insert)
                              ("l" "lambda" jacob-abbrev-no-insert)
                              ("gc" "goto-char" jacob-abbrev-no-insert)
                              ("weal" "with-eval-after-load" jacob-abbrev-no-insert)
                              ("mes" "message" jacob-abbrev-no-insert)
                              ("pmi" "point-min" jacob-abbrev-no-insert)
                              ("pma" "point-max" jacob-abbrev-no-insert)
                              ("int" "(interactive)")
                              ("se" "save-excursion" jacob-abbrev-no-insert))
                            :parents (list jacob-comment-abbrev-table)
                            :enable-function 'jacob-point-in-code-p
                            :regexp "\\(^\\|[\s\t()]\\)\\(?1:[[:alpha:]-]+\\)")
  :bind ( :map lisp-interaction-mode-map
          ("C-j" . jacob-eval-print-last-sexp))
  :custom
  (elisp-flymake-byte-compile-load-path load-path))

(use-package org
  :config
  (defun jacob-org-babel-tangle-delete-newline ()
    "Some code to get rid of the newline org babel likes to add
in when it tangles into a file."
    (goto-char (point-max))
    (delete-trailing-whitespace)
    (backward-delete-char 1)
    (save-buffer))

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((octave . t)
     (sql . t)
     (js . t)))
  :hook (org-babel-post-tangle-hook . jacob-org-babel-tangle-delete-newline)
  :custom
  (org-startup-folded t)
  (org-tags-column 0)
  (org-time-stamp-custom-formats (cons "%A %d/%m/%y" "%A %d/%m/%y %H:%M"))
  (org-display-custom-times t))

(use-package org-src
  :after org
  :custom
  (org-src-preserve-indentation t))

(use-package org-compat
  :after org
  :custom
  (org-calendar-to-agenda-key nil "don't bind calendar key")
  (org-calendar-insert-diary-entry-key nil "don't bind calendar key"))

(use-package ox-latex
  :after org
  :custom
  (org-latex-pdf-process (list "latexmk -pdf %f -shell-escape") "probably requires texlive"))

(use-package org-contrib
  :after org
  :config
  (require 'ox-extra)
  (ox-extras-activate '(latex-header-blocks ignore-headlines)))

(use-package pulse
  :defer
  :init
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
    (advice-add command :after #'jacob-pulse-line)))

(use-package server
  :config
  (server-start))

(use-package smerge-mode
  :bind
  (:repeat-map jacob-smerge-repeat-map
               ("l" . #'smerge-next)
               ("j" . #'smerge-prev)
               ("i" . #'smerge-keep-upper)
               ("k" . #'smerge-keep-lower)
               ("SPC" . #'smerge-keep-all)))

(use-package calendar
  :hook (calendar-today-visible-hook . calendar-mark-today)
  :custom
  (diary-date-forms diary-european-date-forms)
  (calendar-date-style 'european)
  (calendar-date-display-form '((if dayname
                                    (concat dayname ", "))
                                day "/" month "/" year))
  (calendar-week-start-day 1)
  (calendar-mark-diary-entries-flag t)
  (calendar-mark-holidays-flag t))

(provide 'indent)                       ; HACK
(use-package indent
  :config
  (setq-default tab-always-indent 'complete) ; make tab key call indent command or insert tab character, depending on cursor position
  )

(use-package grep
  :defer
  :config
  (when jacob-is-windows
    (setopt find-program "C:/Program Files (x86)/GnuWin32/bin/find.exe")))

(use-package winner
  :after xah-fly-keys
  :init
  (winner-mode 1)
  :bind ( :map xah-fly-command-map
          ("1" . winner-undo)
          ("2" . winner-redo)))

(setopt xah-fly-use-control-key nil
        xah-fly-use-meta-key nil) ; must be set before requiring `xah-fly-keys'

(jacob-require 'xah-fly-keys)

(defun jacob-xfk-define-key-in-major-mode (keymap key command)
  "In KEYMAP bind KEY to COMMAND only when in xfk command mode."
  ;; FIXME: keys that are not already bound will not work for `jacob-xfk-define-key-in-major-mode'
  
  ;; This is a big HACK, because it's time sensitive. It works by
  ;; rebinding a key already bound to something in
  ;; `xah-fly-command-map', but if that key is rebound to something
  ;; else later, it will break the keybinding set up by this
  ;; function. To fix this issue with further hacks, run this
  ;; function in the hook for the major mode.
  (declare (obsolete jacob-xfk-local-key nil))
  (define-key keymap
              (vector 'remap
                      (lookup-key xah-fly-command-map key))
              command))

(defun jacob-xfk-local-key (key command)
  "Bind KEY buffer locally to COMMAND in xfk command mode."
  ;; FIXME: keys that are not already bound will not work for jacob-xfk-define-key-in-major-mode
  (keymap-local-set (format "<remap> <%s>"
                            (keymap-lookup xah-fly-command-map
                                           key))
                    command))

(xah-fly-keys-set-layout "qwerty")
(xah-fly-keys 1)

(defvar-keymap jacob-xfk-map)

(keymap-set xah-fly-leader-key-map "SPC" jacob-xfk-map)
(keymap-set xah-fly-leader-key-map "e p" project-prefix-map)

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
                         (when (comment-search-forward (line-end-position) "NOERROR")
                           (goto-char (match-beginning 0))
                           (skip-syntax-backward " " (line-beginning-position))
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

(fset 'jacob-return-macro [return])

(keymap-global-set "<f7>" #'xah-fly-leader-key-map)

(keymap-global-set "M-SPC" #'xah-fly-command-mode-activate)

(keymap-set xah-fly-command-map "'" #'jacob-format-words)
(keymap-set xah-fly-command-map "-" #'jacob-previous-error-or-punct)
(keymap-set xah-fly-command-map "4" #'other-window-prefix)
(keymap-set xah-fly-command-map "9" #'jacob-swap-visible-buffers)
(keymap-set xah-fly-command-map ";" #'jacob-end-of-line)
(keymap-set xah-fly-command-map "=" #'jacob-next-error-or-punct)
(keymap-set xah-fly-command-map "d" #'jacob-backspace)
(keymap-set xah-fly-command-map "s" #'jacob-return-macro)
(keymap-set xah-fly-command-map "h" #'jacob-beginning-of-line)
(keymap-set xah-fly-command-map "x" #'jacob-kill-line)

(keymap-set xah-fly-insert-map "M-SPC" #'xah-fly-command-mode-activate)

(keymap-set xah-fly-leader-key-map "/ b" #'vc-switch-branch)
(keymap-set xah-fly-leader-key-map "/ c" #'vc-create-branch)
(keymap-set xah-fly-leader-key-map "d h" #'insert-pair)
(keymap-set xah-fly-leader-key-map "d i" #'insert-pair)
(keymap-set xah-fly-leader-key-map "d j" #'insert-pair)
(keymap-set xah-fly-leader-key-map "d k" #'insert-pair)
(keymap-set xah-fly-leader-key-map "d l" #'insert-pair)
(keymap-set xah-fly-leader-key-map "d u" #'insert-pair)
(keymap-set xah-fly-leader-key-map "l 3" #'jacob-async-shell-command)
(keymap-set xah-fly-leader-key-map "l a" #'global-text-scale-adjust)
(keymap-set xah-fly-leader-key-map "u" #'kill-current-buffer)
(keymap-set xah-fly-leader-key-map "w j" #'xref-find-references)
(keymap-set xah-fly-leader-key-map ", n" #'jacob-eval-and-replace)

(require 'compile)
(add-hook 'compilation-filter-hook 'ansi-color-compilation-filter)

(setopt compilation-always-kill t
        compilation-scroll-output t)

(jacob-xfk-define-key-in-major-mode compilation-mode-map "g" #'recompile)

(use-package sql
  :after xah-fly-keys
  :init
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
  :config
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

  (jacob-setup-abbrev-table sql-mode-abbrev-table
                            '(("sel" "SELECT" jacob-abbrev-no-insert)
                              ("upd" "UPDATE" jacob-abbrev-no-insert)
                              ("del" "DELETE FROM ■\nWHERE condition;" jacob-insert)
                              ("joi" "JOIN ■\nON field = field" jacob-insert)
                              ("ins" "INSERT INTO ■ (column, column2)\nVALUES (value, value2)" jacob-insert)
                              ("ord" "ORDER BY")
                              ("gro" "GROUP BY")
                              ("and" "AND")
                              ("as" "AS"))
                            :parents (list jacob-comment-abbrev-table)
                            :enable-function 'jacob-point-in-code-p)
  :hook (sql-interactive-mode-hook . jacob-sql-interactive-mode-hook)
  :bind ( :map jacob-xfk-map
          ("d" . jacob-sql-connect)))

(use-package doc-view
  :hook (doc-view-mode-hook . jacob-doc-view-hook)
  :config
  (defun jacob-doc-view-hook ()
    "hook function for doc view mode"
    (auto-revert-mode 1))

  (when jacob-is-windows
    ;; To get these, install miktex.
    (setopt doc-view-ghostscript-program "mgs.exe")
    (setopt doc-view-pdf->png-converter-function 'doc-view-pdf->png-converter-ghostscript)
    (setopt doc-view-pdftotext-program "miktex-pdftotext.exe")
    (setopt doc-view-dvipdfm-program "dvipdfm.exe")
    ;; To get this, install LibreOffice.
    (setopt doc-view-odf->pdf-converter-program "soffice.exe")
    (setopt doc-view-odf->pdf-converter-function 'doc-view-odf->pdf-converter-soffice))

  (jacob-xfk-define-key-in-major-mode doc-view-mode-map "l" 'doc-view-next-page)
  (jacob-xfk-define-key-in-major-mode doc-view-mode-map "j" 'doc-view-previous-page))

;; JACOBTODO: when next messing with treesit libraries, swap to
;; `treesit-auto'.
(jacob-require 'treesit)
(setopt treesit-language-source-alist '((c-sharp "https://github.com/tree-sitter/tree-sitter-c-sharp" "master" "src")
                                        (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
                                        (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
                                        (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
                                        (yaml "https://github.com/ikatyang/tree-sitter-yaml")
                                        (gdscript "https://github.com/PrestonKnopp/tree-sitter-gdscript"))
        treesit-load-name-override-list '((c-sharp "libtree-sitter-csharp" "tree_sitter_c_sharp"))
        major-mode-remap-alist '((csharp-mode . csharp-ts-mode)
                                 (javascript-mode . js-ts-mode))
        treesit-font-lock-level 4)      ; max level of fontification

(use-package eglot
  :hook ((csharp-ts-mode-hook . eglot-ensure)
         (eglot-managed-mode-hook . jacob-eglot-hook-function))
  :config
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

  (defun jacob-remove-ret-character-from-buffer (&rest _)
    "Remove all occurances of ^M from the buffer.

Useful for deleting ^M after `eglot-code-actions'."
    (save-excursion
      (goto-char (point-min))
      (while (search-forward (char-to-string 13) nil t)
        (replace-match ""))))

  (advice-add 'eglot-code-actions :after #'jacob-remove-ret-character-from-buffer)
  (advice-add 'eglot-rename :after #'jacob-remove-ret-character-from-buffer)

  (defun jacob-eglot-hook-function ()
    "Hook function for `eglot-managed-mode'."
    (eglot-inlay-hints-mode 0)
    (setq-local eldoc-documentation-strategy
                'eldoc-documentation-compose))

  (add-to-list 'eglot-server-programs '((csharp-mode csharp-ts-mode) . ("csharp-ls")))

  (add-to-list 'eglot-server-programs '(sql-mode . "sqls"))

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

  ;; JACOBTODO: is this still required?
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
        (string-trim (buffer-string)))))
  :custom
  (eglot-ignored-server-capabilities '(:documentOnTypeFormattingProvider))
  :bind ( :map jacob-xfk-map
          ("e e" . eglot)
          ("e a" . eglot-code-actions)
          ("e r" . eglot-rename)
          ("e i" . eglot-find-implementation)
          ("e R" . eglot-reconnect)))

(use-package typescript-ts-mode
  ;; JACOBTODO: would it be simpler to use `tsx-ts-mode' for all
  ;; typescript/javascript shenanigans?
  :mode (("\\.ts\\'" . typescript-ts-mode)
         ("\\.tsx\\'" . tsx-ts-mode)))

(use-package message
  :defer
  :custom
  (message-send-mail-function 'smtpmail-send-it))

(jacob-require 'gnus)
(jacob-define-hook-function gnus-started-hook
  (gnus-demon-add-handler 'gnus-demon-scan-news 2 t))

(setopt gnus-use-full-window t
        gnus-always-read-dribble-file t)

(keymap-set jacob-xfk-map "g" #'gnus)

(jacob-require 'gnus-group)
(jacob-define-hook-function gnus-group-mode-hook
  (jacob-xfk-local-key "q" #'gnus-group-exit)
  (jacob-xfk-local-key "i" #'gnus-group-prev-group)
  (jacob-xfk-local-key "k" #'gnus-group-next-group)
  (jacob-xfk-local-key "g" #'gnus-group-get-new-news))

(use-package gnus-notifications
  :hook (gnus-after-getting-new-news-hook . gnus-notifications))

(jacob-require 'gnus-sum)
(jacob-define-hook-function gnus-summary-mode-hook
  (jacob-xfk-local-key "q" #'gnus-summary-exit)
  (jacob-xfk-local-key "i" #'gnus-summary-prev-article)
  (jacob-xfk-local-key "k" #'gnus-summary-next-article)
  (jacob-xfk-local-key "j" #'gnus-summary-prev-page)
  (jacob-xfk-local-key "l" #'gnus-summary-next-page))

(use-package gnus-topic
  :hook (gnus-group-mode-hook . gnus-topic-mode)
  :config
  (defun jacob-gnus-topic-mode-hook-function ()
    "Hook function to be used with `gnus-topic-mode-hook'."
    (jacob-xfk-local-key "s" #'gnus-topic-select-group)))

(use-package nxml-mode
  :mode (("\\.csproj\\'" . nxml-mode)
         ("Directory.Packages.props" . nxml-mode)))

(jacob-require 'key-chord)
(key-chord-mode 1)

(jacob-require 'avy)

(defun jacob-avy-action-xref (pt)
  (save-excursion
    (goto-char pt)
    (call-interactively #'xref-find-definitions))
  (select-window
   (cdr (ring-ref avy-ring 0)))
  t)

(setopt avy-keys '(?a ?s ?d ?f ?g ?h ?j ?l ?\;)
        avy-dispatch-alist '((?y . avy-action-yank)
                             (?k . avy-action-kill-stay)
                             (?K . avy-action-kill-move)
                             (?t . avy-action-teleport)
                             (?m . avy-action-mark)
                             (?w . avy-action-copy)
                             (?i . avy-action-ispell)
                             (?z . avy-action-zap-to-char)
                             (?. . jacob-avy-action-xref)))

(key-chord-define-global "fj" #'avy-goto-char-timer)

(use-package apheleia
  :ensure
  :config
  (apheleia-global-mode 1)
  (setq-default apheleia-inhibit t) ; set `apheleia-inhibit' to nil to enable
  ;; JACOBTODO: how does add-to-list work?
  (push '(csharpier "dotnet" "csharpier" "--write-stdout")
        apheleia-formatters)
  (push '(csharp-ts-mode . csharpier)
        apheleia-mode-alist)

  (defun jacob-apheleia-skip-function ()
    "Function for `apheleia-skip-functions'. If point is in
a yasnippet field or the minibuffer is active, do not format the
buffer."
    (or (seq-find (lambda (overlay)
                    (overlay-get overlay 'yas--snippet))
                  (overlays-at (point)))
        (minibuffer-window-active-p (car (window-list)))))
  
  (add-to-list 'apheleia-skip-functions #'jacob-apheleia-skip-function))

(jacob-require 'rainbow-mode)
(add-hook 'prog-mode-hook #'rainbow-mode)

(jacob-require 'eglot-booster "https://github.com/jdtsmith/eglot-booster")
(eglot-booster-mode 1)

(jacob-require 'csharp-toolbox "https://github.com/lem102/csharp-toolbox.git")
;; JACOBTODO: can i make this use ssh?
;; JACOBTODO: how should i load this properly? 🤔
(keymap-set jacob-xfk-map "c f" #'csharp-toolbox-format-statement)
(keymap-set jacob-xfk-map "c t" #'csharp-toolbox-run-test)
(keymap-set jacob-xfk-map "c a" #'csharp-toolbox-toggle-async)
(keymap-set jacob-xfk-map "c n" #'csharp-toolbox-guess-namespace)
(keymap-set jacob-xfk-map "c ;" #'csharp-toolbox-wd40)

(use-package dape
  :ensure
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
          :program csharp-toolbox--select-dll
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
  :ensure
  ;; JACOBTODO: investigate switch window finish hook to solve compilation scroll issue
  :after xah-fly-keys
  :custom
  (switch-window-shortcut-style 'qwerty)
  (switch-window-threshold 3)
  :bind ( :map xah-fly-command-map
          ("," . switch-window)))

(use-package tex                        ; auctex is weird
  :ensure auctex
  :mode ("\\.tex\\$" . latex-mode)
  :config
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq-default japanese-TeX-error-messages nil)
  (TeX-global-PDF-mode 0))

(jacob-require 'fsharp-mode)
(setopt inferior-fsharp-program "dotnet fsi")

(jacob-require 'eglot-fsharp)
(setopt eglot-fsharp-server-install-dir nil)

(use-package vertico
  :ensure
  :init
  (vertico-mode 1)
  (vertico-mouse-mode 1)
  :custom
  (vertico-count 25))

(use-package orderless
  :ensure
  :after vertico
  :custom
  (completion-styles '(orderless initials)))

(use-package marginalia
  :ensure
  :after vertico
  :config
  (marginalia-mode 1))

(use-package consult
  :ensure
  :after xah-fly-keys vertico
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
    (add-to-list 'consult-buffer-sources jacob-consult-project-source "APPEND"))
  :bind (("M-g i" . consult-imenu) 
         :map xah-fly-leader-key-map
         ("v" . consult-yank-from-kill-ring)
         ("f" . consult-buffer)
         ("ij" . consult-recent-file)
         ("es" . consult-line)
         ("ku" . consult-goto-line)
         :map project-prefix-map
         ("g" . jacob-project-search)))

(jacob-require 'embark)

(keymap-set xah-fly-command-map "\\" #'embark-act)
(setopt embark-cycle-key "\\"
        embark-help-key "h")

(use-package expand-region
  :ensure
  :after xah-fly-keys
  :custom
  (expand-region-contract-fast-key "9")
  :bind ( :map xah-fly-command-map
          ("8" . er/expand-region)))

(use-package verb
  :ensure
  :hook (org-mode-hook . verb-mode)
  :config
  (jacob-xfk-define-key-in-major-mode verb-response-body-mode-map "q" #'quit-window))

(jacob-require 'sly)

(jacob-define-hook-function sly-db-hook
  (jacob-xfk-local-key "q" #'sly-db-quit))

(sly-symbol-completion-mode 0)

(jacob-xfk-define-key-in-major-mode lisp-mode-map " ,m" #'sly-eval-last-expression)
(jacob-xfk-define-key-in-major-mode lisp-mode-map " ,d" #'sly-compile-defun)
(jacob-xfk-define-key-in-major-mode lisp-mode-map " ,e" #'sly-eval-buffer)
(jacob-xfk-define-key-in-major-mode lisp-mode-map " wk" #'sly-edit-definition)

(jacob-require 'sql-indent)

(use-package yasnippet
  :ensure
  :demand
  :hook (snippet-mode-hook . jacob-snippet-mode-hook)
  :config
  (yas-global-mode 1)
  (keymap-set yas-minor-mode-map "SPC" yas-maybe-expand)

  (defun jacob-snippet-mode-hook ()
    "Function to be run in hook for `snippet-mode'."
    (setq-local auto-save-visited-mode nil))

  (defun jacob-autoinsert-yas-expand ()
    "Replace text in yasnippet template."
    (yas-expand-snippet (buffer-string) (point-min) (point-max)))

  ;; JACOBTODO: move to csharp use package
  (define-auto-insert "\\.cs$" [ "template.cs" jacob-autoinsert-yas-expand ])

  (defun jacob-yas-camel-case (input)
    "Convert INPUT to camel case e.g. apple banana -> appleBanana.
For use in yasnippets."
    (let* ((space-at-end (if (string-match-p " $" input) " " ""))
           (words (split-string input))
           (capitalised-words (seq-reduce (lambda (previous current)
                                            (concat previous (capitalize current)))
                                          (cdr words)
                                          (car words))))
      (concat capitalised-words space-at-end)))

  (defun jacob-yas-pascal-case (input)
    "Convert INPUT to pascal case e.g. apple banana -> AppleBanana.
For use in yasnippets."
    (let ((space-at-end (if (string-match-p " $" input)
                            " "
                          "")))
      (with-temp-buffer
        (insert input)
        (goto-char (point-min))
        (subword-mode 1)
        (while (not (= (point) (point-max)))
          (call-interactively #'capitalize-word))
        (goto-char (point-min))
        (while (search-forward " " nil "NOERROR")
          (replace-match ""))
        (goto-char (point-max))
        (insert space-at-end)
        (buffer-substring-no-properties (point-min) (point-max)))))

  (defun jacob-yas-snake-case (input)
    "Convert INPUT to snake case e.g. apple banana -> apple_banana.
For use in yasnippets."
    (string-replace " " "_" input))

  (defun jacob-yas-kebab-case (input)
    "Convert INPUT to kebab case e.g. apple banana -> apple_banana.
For use in yasnippets."
    (string-replace " " "-" input)))

(use-package aas
  :ensure
  :hook ((emacs-lisp-mode-hook csharp-ts-mode-hook gdscript-mode-hook) . aas-activate-for-major-mode)
  :config
  (aas-set-snippets 'emacs-lisp-mode
    :cond #'jacob-point-in-code-p
    "pmi" "(point-min)"
    "pma" "(point-max)"
    "gc" '(yas "(goto-char $0)"))
  (aas-set-snippets 'csharp-ts-mode
    :cond #'jacob-point-in-code-p
    "nuid" "Guid.NewGuid()"
    "var" '(yas "var ${1:x$(jacob-yas-camel-case yas-text)} = $0")
    "pub" "public"
    ";az" "async"
    ";aw" "await"
    "aqbm" ".AsQueryable().BuildMock()")
  (aas-set-snippets 'gdscript-mode
    :cond #'jacob-point-in-code-p
    "v2" "Vector2"
    "var" '(yas "var ${1:x$(jacob-yas-snake-case yas-text)} = $0")))

(jacob-require 'gptel)

(use-package gdscript-mode
  :ensure
  :defer
  :config
  (jacob-setup-abbrev-table gdscript-mode-abbrev-table
                            '(("v" "var" jacob-abbrev-no-insert)
                              ("c" "const" jacob-abbrev-no-insert)
                              ("v2" "Vector2")
                              ("ret" "return"))
                            :parents (list jacob-comment-abbrev-table)
                            :enable-function 'jacob-point-in-code-p)
  (push '(gdscript-mode "localhost" 6008) eglot-server-programs))

(use-package highlight-defined
  :ensure
  :hook (emacs-lisp-mode-hook . highlight-defined-mode))

(use-package lisp-extra-font-lock
  :ensure
  :config
  (lisp-extra-font-lock-global-mode 1))


;; personal functions

(defun jacob-indent-buffer ()
  "Indent whole buffer. Designed for use in `before-save-hook'."
  (unless (ignore-errors smerge-mode)
    (indent-region (point-min) (point-max))))

(define-minor-mode jacob-screen-sharing-mode
  "Minor mode for sharing screens."
  :global t
  (let ((on (if jacob-screen-sharing-mode 1 0)))
    (global-hl-line-mode on)
    (global-display-line-numbers-mode on)))

(defun jacob-ip-to-kill-ring ()
  "Copy v4 ip address to kill ring."
  (interactive)
  (kill-new (with-temp-buffer (shell-command "curl --silent -4 ifconfig.me"
                                             t)
                              (buffer-string))))

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

(defun jacob-alist-to-form-data (alist)
  "Convert ALIST to form-data for http request."
  (mapconcat (lambda (x)
               (concat (car x) "=" (cdr x)))
             alist
             "&"))

(defun jacob-toggle-modeline ()
  "Toggle visibility of modeline."
  (interactive)
  (if mode-line-format
      (setq mode-line-format nil)
    (set 'mode-line-format (eval
                            (car
                             (get 'mode-line-format
                                  'standard-value))))))

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

;; jacob-insert-config

;; JACOBTODO: remove when switched to yasnippet

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

(define-abbrev-table 'text-mode-abbrev-table
  '(("i" "I")
    ("im" "I'm")
    ("idd" "I'd")
    ("dont" "don't")
    ("its" "it's")
    ("havent" "haven't")))

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


;; keybindings


;; macros


;; xah-fly-keys keybindings

(with-eval-after-load 'xah-fly-keys

  (let ((map minibuffer-local-completion-map))
    (define-key map "SPC" 'self-insert-command))

  (let ((map occur-mode-map))
    (jacob-xfk-define-key-in-major-mode map "q" 'quit-window)
    (jacob-xfk-define-key-in-major-mode map "i" 'occur-prev)
    (jacob-xfk-define-key-in-major-mode map "k" 'occur-next))

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

  

  (with-eval-after-load 'csharp-mode
    (let ((map csharp-ts-mode-map))
      ;; (jacob-xfk-define-key-in-major-mode map "h" 'jacob-csharp-beginning-of-line-or-statement)
      ;; (jacob-xfk-define-key-in-major-mode map ";" 'jacob-csharp-end-of-line-or-statement)
      ))

  (with-eval-after-load 'js
    (jacob-xfk-define-key-in-major-mode js-ts-mode-map " ,c" #'recompile))

  (with-eval-after-load 'csharp-mode
    (jacob-xfk-define-key-in-major-mode csharp-ts-mode-map " ,c" #'recompile))

  (with-eval-after-load 'typescript-ts-mode
    (jacob-xfk-define-key-in-major-mode typescript-ts-mode-map " ,c" #'recompile)))

(provide 'init)
;;; init.el ends here
