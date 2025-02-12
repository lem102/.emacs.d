;;; init.el --- Jacob's main init file. -*-lexical-binding: t-*-
;;; Commentary:
;;; Code:

;; read environment file and variable setup

(defvar jacob-font-size 11
  "Font size to use.")

(defconst jacob-is-windows (eq system-type 'windows-nt)
  "Is the current OS windows?")

(defconst jacob-is-linux (eq system-type 'gnu/linux)
  "Is the current OS linux?")

(when (file-exists-p "~/.emacs.d/environment.el")
  (load-file "~/.emacs.d/environment.el"))

;; c source code
(setq-default truncate-lines nil)
(setq-default tab-width 4) ; set default tab char's display width to 4 spaces

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
        truncate-partial-width-windows nil
        enable-recursive-minibuffers t
        completion-ignore-case t
        kill-buffer-query-functions (delq 'process-kill-buffer-query-function
                                          kill-buffer-query-functions)
        echo-keystrokes 0.01)

;; startup
(setopt inhibit-startup-screen t
        initial-scratch-message (format ";; %s\n\n"
                                        (seq-random-elt
                                         '("\"A journey of a thousand miles begins with a single step.\" - 老子"
                                           "\"apex predator of grug is complexity\" - some grug"
                                           "\"Perfection is achieved, not when there is nothing more to add, but when there is nothing left to take away.\" - Antoine de Saint-Exupéry"
                                           "\"Always listen to Jiaqi.\" - Jacob Leeming"
                                           "\"The king wisely had the computer scientist beheaded, and they all lived happily ever after.\" - anon"
                                           "\"Success is going from failure to failure without losing your enthusiasm.\" - Winston Churchill (maybe)"))))

;; lisp
(setopt parens-require-spaces nil
        delete-pair-blink-delay 0
        insert-pair-alist (append insert-pair-alist
                                  '((?k ?\( ?\))
                                    (?l ?\[ ?\])
                                    (?j ?\{ ?\})
                                    (?u ?\" ?\")
                                    (?i ?\' ?\')
                                    (?h ?\< ?\>))))

;; mule-cmds
(prefer-coding-system 'utf-8)

;; bindings
(setopt mode-line-percent-position nil)

(require 'package)

(defvar jacob-require-already-refreshed nil
  "If nil, haven't refreshed packages with `jacob-require' yet.")

(defun jacob-ensure-installed (package &optional vc)
  "Ensure PACKAGE is installed.

If VC is provided, it is passed to `package-vc-install' to
install the package rather than using `package-install'."
  (unless (package-installed-p package)
    (if vc
        (package-vc-install vc)
      (unless jacob-require-already-refreshed
        (package-refresh-contents)
        (setq jacob-require-already-refreshed t))
      (package-install package))))

(defmacro jacob-require (package &optional vc)
  "Ensure the PACKAGE is installed, then `require' it.

VC is used in `jacob-ensure-installed'."
  `(progn
     (jacob-ensure-installed ,package ,vc)
     (setopt elisp-flymake-byte-compile-load-path load-path) ; make flymake aware of new package
     (require ,package)))

(setopt package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                           ("nongnu" . "https://elpa.nongnu.org/nongnu/")
                           ("melpa" . "https://melpa.org/packages/")))

(defmacro jacob-defhookf (hook &rest body)
  "Define function with BODY and bind it to HOOK."
  (declare (indent defun))
  (let* ((hook-name (symbol-name hook))
         (function-name (intern (concat "jacob-" hook-name "-function"))))
    `(progn
       (defun ,function-name ()
         ,(format "Auto-generated hook function for `%s'." hook-name)
         ,@body)
       (add-hook ',hook #',function-name))))

(jacob-require 'no-littering)

(jacob-require 'delight)

(jacob-require 'which-key)         ; in vanilla by V30.1
(setopt which-key-idle-delay 0.01) ; needs to be set before loading the mode
(which-key-mode 1)
(delight 'which-key-mode nil t)

(require 'mwheel)

(setopt mouse-wheel-progressive-speed nil
        mouse-wheel-scroll-amount '(10 ((shift) . hscroll)
                                       ((meta))
                                       ((control) . text-scale)))

(require 'tooltip)

(setopt tooltip-delay 0.1)

(require 'files)
(auto-save-visited-mode 1)
(keymap-global-unset "C-x C-c")         ; `save-buffers-kill-terminal'

(setopt auto-save-default nil
        make-backup-files nil
        backup-by-copying t
        confirm-kill-processes nil
        auto-save-visited-interval 2)   ; save file after two seconds

(require 'autorevert)
(delight 'auto-revert-mode nil t)

(require 'window)

(setopt switch-to-buffer-obey-display-actions t
        display-buffer-alist '(((major-mode . sql-interactive-mode)
                                (display-buffer-reuse-mode-window display-buffer-same-window))
                               ((major-mode . prodigy-mode)
                                (display-buffer-reuse-mode-window display-buffer-same-window)))
        split-height-threshold nil)

(defvar-keymap jacob-recenter-repeat-map
  :repeat t
  "p" #'recenter-top-bottom)

(require 'frame)
(setopt blink-cursor-blinks 0)          ; make cursor blink forever
(keymap-global-unset "C-z")             ; `suspend-frame'

(require 'novice)
(setopt disabled-command-function nil)

(require 'recentf)
(recentf-mode 1)

(require 'savehist)
(savehist-mode 1)
(setopt savehist-save-minibuffer-history t)

(require 'saveplace)
(save-place-mode 1)
(setopt save-place-forget-unreadable-files t)

(require 'custom)
(load-theme 'modus-vivendi)

(require 'tool-bar)
(tool-bar-mode 0)

(require 'menu-bar)
(menu-bar-mode 0)

(require 'scroll-bar)
(scroll-bar-mode 0)

(require 'cus-edit)
(setopt custom-file (make-temp-file "emacs-custom-"))

(require 'generic-x)             ; support for files like `/etc/fstab'

(require 'simple)
(column-number-mode 1)
(line-number-mode 1)
(setq-default indent-tabs-mode nil)     ; use spaces to indent
(setopt save-interprogram-paste-before-kill t)
(keymap-global-unset "C-x u")           ; `undo'

(require 'bookmark)
(bookmark-store "emacs init file" '((filename . "~/.emacs.d/init.el")) nil)
(bookmark-store "emacs environment file" '((filename . "~/.emacs.d/environment.el")) nil)
(setopt bookmark-set-fringe-mark nil
        bookmark-watch-bookmark-file 'silent)

(require 'flymake)

(setopt xah-fly-use-control-key nil
        xah-fly-use-meta-key nil) ; must be set before requiring `xah-fly-keys'

(jacob-require 'xah-fly-keys)

(delight 'xah-fly-keys)

(defun jacob-xfk-local-key (key command)
  "Bind KEY buffer locally to COMMAND in xfk command mode."
  (let ((existing-command (keymap-lookup xah-fly-command-map key nil "NO-REMAP")))
    (unless existing-command
      (user-error "%s is not bound to a key in `xah-fly-command-map'" key))
    (keymap-local-set (format "<remap> <%s>" existing-command)
                      command)))

(xah-fly-keys-set-layout "qwerty")
(xah-fly-keys 1)

(defvar-keymap jacob-xfk-map)

(keymap-set xah-fly-leader-key-map "SPC" jacob-xfk-map)
(keymap-set jacob-xfk-map "p" `("Project" . ,project-prefix-map))

(defvar-local jacob-backspace-function nil
  "Called by `jacob-backspace' if non-nil.")

(defun jacob-backspace ()
  "DWIM backspace command.

If character to the left is a pair character as determined by
`insert-pair-alist', kill from the pair to its match.  If the
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
  "If region is active, kill it.  Otherwise:

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

(defun jacob-kill-paragraph ()
  "Move to the beginning of the paragraph, then kill it."
  (interactive)
  (forward-paragraph)
  (backward-paragraph)
  (kill-paragraph 1))

(defalias 'jacob-return-macro
  (kmacro "<return>"))

(keymap-global-set "<f7>" #'xah-fly-leader-key-map)

(keymap-global-set "M-SPC" #'xah-fly-command-mode-activate)

(keymap-set xah-fly-command-map "'" #'jacob-format-words)
(keymap-set xah-fly-command-map "-" #'flymake-goto-prev-error)
(keymap-set xah-fly-command-map "4" #'other-window-prefix)
(keymap-set xah-fly-command-map "9" #'jacob-swap-visible-buffers)
(keymap-set xah-fly-command-map ";" #'jacob-end-of-line)
(keymap-set xah-fly-command-map "=" #'flymake-goto-next-error)
(keymap-set xah-fly-command-map "d" #'jacob-backspace)
(keymap-set xah-fly-command-map "g" #'jacob-kill-paragraph)
(keymap-set xah-fly-command-map "h" #'jacob-beginning-of-line)
(keymap-set xah-fly-command-map "s" #'jacob-return-macro)
(keymap-set xah-fly-command-map "x" #'jacob-kill-line)

(keymap-set xah-fly-insert-map "M-SPC" #'xah-fly-command-mode-activate)

(keymap-set xah-fly-leader-key-map "/ b" #'vc-switch-branch)
(keymap-set xah-fly-leader-key-map "/ c" #'vc-create-branch)
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

(jacob-require 'yasnippet)

(defun jacob-point-in-text-p ()
  "Return t if in comment or string.  Else nil."
  (let ((xsyntax-state (syntax-ppss)))
    (or (nth 3 xsyntax-state)
        (nth 4 xsyntax-state))))

(defun jacob-point-in-code-p ()
  "Return t if outside of string or comment.  Else nil."
  (not (jacob-point-in-text-p)))

(jacob-defhookf snippet-mode-hook
  (setq-local auto-save-visited-mode nil))

(yas-global-mode 1)

(setopt yas-new-snippet-default "# -*- mode: snippet -*-
# key: $1
# --
$0`(yas-escape-text yas-selected-text)`")

(defun jacob-autoinsert-yas-expand ()
  "Replace text in yasnippet template."
  (yas-expand-snippet (buffer-string) (point-min) (point-max)))

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
  (string-replace " " "-" input))

(delight 'yas-minor-mode nil t)

(defvar-keymap jacob-yas-map
  "n" #'yas-new-snippet
  "v" #'yas-visit-snippet-file)

(keymap-set jacob-xfk-map "y" `("Yasnippet" . ,jacob-yas-map))

(require 'minibuffer)
(define-key minibuffer-local-completion-map "SPC" 'self-insert-command)

(jacob-defhookf minibuffer-setup-hook
  (jacob-xfk-local-key "g" #'embark-export))

(require 'replace)
(jacob-defhookf occur-mode-hook
  (jacob-xfk-local-key "q" 'quit-window)
  (jacob-xfk-local-key "i" 'occur-prev)
  (jacob-xfk-local-key "k" 'occur-next))

(require 'info)
(jacob-defhookf Info-mode-hook
  (jacob-xfk-local-key "q" 'quit-window)
  (jacob-xfk-local-key "r" 'Info-scroll-up)
  (jacob-xfk-local-key "e" 'Info-scroll-down)
  (jacob-xfk-local-key "w" 'Info-up)
  (jacob-xfk-local-key "g" 'Info-menu))

(require 'diff-mode)
(jacob-defhookf diff-mode-hook
  (jacob-xfk-local-key "q" #'quit-window)
  (jacob-xfk-local-key "e" #'diff-hunk-prev)
  (jacob-xfk-local-key "r" #'diff-hunk-next)
  (jacob-xfk-local-key "x" #'diff-hunk-kill)
  (jacob-xfk-local-key "g" #'revert-buffer))

(require 'help)
(setopt help-window-select t
        help-enable-variable-value-editing t)

(require 'help-fns)
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

(require 'help-mode)
(jacob-defhookf help-mode-hook
  (jacob-xfk-local-key "s" #'help-view-source)
  (jacob-xfk-local-key "q" #'quit-window)
  (jacob-xfk-local-key "e" #'help-go-back)
  (jacob-xfk-local-key "r" #'help-go-forward)
  (jacob-xfk-local-key "g" #'revert-buffer)
  (jacob-xfk-local-key "w" #'jacob-help-edit))

(jacob-require 'helpful)

(keymap-set xah-fly-leader-key-map "j k" #'helpful-callable)
(keymap-set xah-fly-leader-key-map "j l" #'helpful-variable)
(keymap-set xah-fly-leader-key-map "j v" #'helpful-key)
(keymap-set xah-fly-leader-key-map "j b" #'helpful-command)

(jacob-defhookf helpful-mode-hook
  (jacob-xfk-local-key "q" #'quit-window)
  (jacob-xfk-local-key "g" #'helpful-update)
  (jacob-xfk-local-key "e" #'backward-button)
  (jacob-xfk-local-key "r" #'forward-button)
  (jacob-xfk-local-key "s" #'push-button))

(require 'help-at-pt)
(setq-default help-at-pt-display-when-idle '(flymake-diagnostic))
(help-at-pt-set-timer)

(require 'warnings)
(setopt warning-minimum-level :error)

(require 'subword)
(global-subword-mode 1)
(delight 'subword-mode nil t)

(require 'paren)
(show-paren-mode 1)
(setopt show-paren-when-point-inside-paren t)

(require 'elec-pair)
(electric-pair-mode 1)

(require 'delsel)
(delete-selection-mode 1)

(require 'repeat)
(repeat-mode 1)

(require 'dabbrev)
(setopt dabbrev-case-fold-search nil
        dabbrev-case-replace nil)

(require 'vc)
(setopt vc-git-show-stash 0             ; show 0 stashes
        vc-ignore-dir-regexp (format "\\(%s\\)\\|\\(%s\\)" ; ignore tramp files
                                     vc-ignore-dir-regexp
                                     tramp-file-name-regexp))

(require 'vc-git)
(jacob-defhookf vc-git-log-view-mode-hook
  (jacob-xfk-local-key "q" #'quit-window))

(require 'vc-dir)
(jacob-defhookf vc-dir-mode-hook
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
  (jacob-xfk-local-key ";" #'jacob-git-push-set-upstream)
  (jacob-xfk-local-key "=" #'vc-diff)
  (jacob-xfk-local-key "x" #'vc-dir-hide-up-to-date))

(require 'vc-annotate)
(jacob-defhookf vc-annotate-mode-hook
  (jacob-xfk-local-key "q" #'quit-window)
  (jacob-xfk-local-key "g" #'revert-buffer))

(jacob-require 'magit)

(jacob-require 'git-gutter-fringe)
(global-git-gutter-mode 1)
(delight 'git-gutter-mode nil t)

(require 'autoinsert)
(auto-insert-mode t)
(setopt auto-insert-query t)

(require 'tramp)
(add-to-list 'tramp-remote-path 'tramp-own-remote-path)
(setopt tramp-archive-enabled nil) ; lots of problems. for now, disable it!

(require 'eglot)

(jacob-defhookf eglot-managed-mode-hook
  (eglot-inlay-hints-mode 0)
  (setq-local eldoc-documentation-strategy 'eldoc-documentation-compose))

;; TODO: function that can smartly decide between jumping to
;; definition or implementation (`xref-find-definitions' vs
;; `eglot-find-implementation')

;; WIP
(defun jacob-go-definition ()
  "If not in an eglot buffer, do regular xref stuff.

Otherwise, go to implementation.  If already at implementation go to
definition."
  (interactive)
  (if (not eglot--managed-mode)
      (call-interactively #'xref-find-definitions)
    (let ((start-buffer (current-buffer)))
      (ignore-errors
        (eglot-find-implementation))
      (when (eq start-buffer (current-buffer))
        ;; TODO: won't work, this just takes us to the current
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

(setopt eglot-ignored-server-capabilities '(:documentOnTypeFormattingProvider))

(defvar-keymap jacob-code-map
  "e" #'eglot
  "a" #'eglot-code-actions
  "r" #'eglot-rename
  "i" #'eglot-find-implementation
  "t" #'eglot-find-typeDefinition)

(keymap-set jacob-xfk-map "c" `("Code" . ,jacob-code-map))

(require 'csharp-mode)

(defun jacob-csharp-create-variable ()
  "Create a variable declaration statement for an undeclared variable."
  (interactive)
  (let* ((identifier
          (thing-at-point 'symbol "NO-PROPERTIES"))
         (first-occurance
          (seq-first (seq-sort #'<
                               (mapcar #'treesit-node-start
                                       (mapcar #'cdr
                                               (treesit-query-capture (csharp-toolbox--get-method-node)
                                                                      `(((identifier) @id (:equal @id ,identifier))))))))))
    (goto-char first-occurance)
    (goto-char (treesit-beginning-of-thing "_statement$"))
    (forward-line -1)
    (end-of-line)
    (newline 1 "INTERACTIVE")
    (insert (format "var %s = Guid.NewGuid();" identifier))))

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
  (if (eq last-command this-command)
      (call-interactively 'jacob-csharp-forward-statement)
    (end-of-line)))

(defun jacob-backspace-csharp (f)
  "Function for `jacob-backspace' to help with csharp.

Figure out if the `<' or `>' before point is part of a
`type_argument_list', and delete accordingly.  F is a function
which performs the deletion."
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

;; TODO: include only my modifications rather than the whole data structure
(setopt csharp-ts-mode--indent-rules
        '((c-sharp
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

;; TODO: merge into emacs core
(nconc csharp-ts-mode--font-lock-settings
       (treesit-font-lock-rules
        :language 'c-sharp
        :feature 'property
        :override t
        `((property_declaration
           type: (generic_name name: (identifier) @font-lock-type-face)
           name: (identifier) @font-lock-variable-name-face))))

(add-to-list 'compilation-error-regexp-alist-alist
             '(jacob-dotnet-stacktrace-re
               "   at [^
]+ in \\(.+\\):line \\([[:digit:]]+\\)"
               1
               2))

(add-to-list 'compilation-error-regexp-alist 'jacob-dotnet-stacktrace-re)

(jacob-defhookf csharp-ts-mode-hook
  (setq treesit-defun-type-regexp "\\(method\\|constructor\\|field\\)_declaration")
  (setq jacob-backspace-function #'jacob-backspace-csharp)
  (eglot-ensure))

(define-auto-insert "\\.cs$" ["template.cs" jacob-autoinsert-yas-expand])

;; TODO: test the below code!

(defun eglot-csharp-ls-metadata (xrefs)
  "Advice for `eglot--lsp-xrefs-for-method'.

- For showing `csharp-ls' metadata in Emacs.

- Check the first xref from XREFS to see if it's referring to a
  non-existant metadata file.

- If so, create it in a “sensible” location and modify the xref to
  point there."
  (dolist (xref-match-item xrefs)
    (when-let ((xref-file-location (xref-item-location xref-match-item))
               (uri (xref-file-location-file xref-file-location))
               (uri-path (when (string-match "^csharp:\\(.*\\)$" uri)
                           (match-string 1 uri)))
               (target-path (concat (directory-file-name (project-root (project-current)))
                                    uri-path))
               (source (plist-get (eglot--request (eglot-current-server)
                                                  :csharp/metadata
                                                  `(:textDocument (:uri ,uri)))
                                  :source)))
      (with-temp-buffer
        (insert source)
        (write-file target-path nil))

      (setf (xref-file-location-file xref-file-location)
            target-path)))
  xrefs)

(advice-add #'eglot--lsp-xrefs-for-method :filter-return #'eglot-csharp-ls-metadata)

(jacob-require 'sharper)

(keymap-set jacob-xfk-map "d" #'sharper-main-transient)

(jacob-require 'csproj-mode)

;; TODO: package `sln-mode' for elpa/melpa?
(jacob-require 'font-lock-ext "https://github.com/sensorflo/font-lock-ext.git") ; dependency of `sln-mode'
(jacob-require 'sln-mode "https://github.com/sensorflo/sln-mode.git")
(add-to-list 'auto-mode-alist '("\\.sln\\'". sln-mode))

(jacob-require 'fsharp-mode)

(remove-hook 'project-find-functions #'fsharp-mode-project-root)

(require 'inf-lisp)
(setopt inferior-lisp-program "sbcl")

(require 'dired)
(jacob-defhookf dired-mode-hook
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

(require 'dired-aux)
(setopt dired-vc-rename-file t)

(require 'ls-lisp)
(setopt ls-lisp-use-insert-directory-program nil
        ls-lisp-dirs-first t)

(jacob-require 'dired-rsync)

(add-to-list 'mode-line-misc-info '(:eval dired-rsync-modeline-status 'append))

(require 'esh-mode)

(setopt eshell-scroll-to-bottom-on-output t)

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
    (split-string (buffer-substring-no-properties (point-min) (point-max)) "\n")))

(require 'pcomplete)

(defun pcomplete/gco ()
  "Completion for the gco alias on git branches."
  (pcomplete-here* (jacob-git-get-branches)))

(defun pcomplete/grh ()
  "Completion for the grh alias on git branches."
  (pcomplete-here* (jacob-git-get-branches t)))

(require 'esh-proc)

(defun jacob-confirm-terminate-batch-job ()
  "Type y and enter to terminate batch job after sending ^C."
  (when (not (null eshell-process-list))
    (insert "y")
    (eshell-send-input)))

(when jacob-is-windows
  (advice-add 'eshell-interrupt-process :after #'jacob-confirm-terminate-batch-job))

(jacob-require 'eat)
(add-hook 'eshell-mode-hook #'eat-eshell-mode)

(require 'eldoc)
(global-eldoc-mode 1)
(delight 'eldoc-mode nil t)
(setopt eldoc-documentation-strategy 'eldoc-documentation-compose)

(require 'project)
(setopt project-switch-commands '((project-find-file "Find file")
                                  (jacob-project-search "Find regexp")
                                  (project-find-dir "Find directory")
                                  (project-vc-dir "VC-Dir")
                                  (project-eshell "Eshell")
                                  (project-compile "Compile")))

(jacob-require 'prodigy)

(setopt prodigy-kill-process-buffer-on-stop t)

(prodigy-define-tag
  :name 'asp.net
  :stop-signal 'kill
  :on-output (lambda (&rest args)
               (let ((output (plist-get args :output))
                     (service (plist-get args :service)))
                 (when (string-match-p "Hosting started *$" output)
                   (prodigy-set-status service 'ready)))))

(keymap-set project-prefix-map "l" #'prodigy)

(jacob-defhookf prodigy-mode-hook
  (hl-line-mode 0)
  (jacob-xfk-local-key "d" #'prodigy-stop)
  (jacob-xfk-local-key "e" #'prodigy-mark)
  (jacob-xfk-local-key "g" #'jacob-project-search)
  (jacob-xfk-local-key "f" #'project-find-file)
  (jacob-xfk-local-key "i" #'prodigy-prev)
  (jacob-xfk-local-key "k" #'prodigy-next)
  (jacob-xfk-local-key "q" #'quit-window)
  (jacob-xfk-local-key "r" #'prodigy-unmark)
  (jacob-xfk-local-key "s" #'prodigy-restart)
  (jacob-xfk-local-key "v" #'prodigy-display-process))

(jacob-defhookf prodigy-view-mode-hook
  (compilation-minor-mode 1)
  (jacob-xfk-local-key "q" #'quit-window)
  (jacob-xfk-local-key "g" #'prodigy-restart))

(require 'hi-lock)
(delight 'hi-lock-mode nil t)

(jacob-require 'highlight-defined)

(jacob-require 'hl-todo)
(global-hl-todo-mode 1)

(jacob-require 'lisp-extra-font-lock)
(lisp-extra-font-lock-global-mode 1)

(require 'elisp-mode)

(jacob-defhookf emacs-lisp-mode-hook
  (flymake-mode 1)
  (highlight-defined-mode 1)
  (add-hook 'before-save-hook 'jacob-indent-buffer nil "LOCAL")
  (setq-local yas-key-syntaxes '("w_")))

(add-to-list 'lisp-imenu-generic-expression '("Features" "^(\\(jacob-\\)*require '\\([a-z-]+\\)" 2))

(yas-define-snippets #'emacs-lisp-mode
                     '(("add-hook" "(add-hook '-hook$0 #')")
                       ("cond" "(cond ($0t 0))")
                       ("let" "(let (($0x 1))\n)")
                       ("save-excursion" "(save-excursion\n$0)")
                       ("defun" "(defun $0 ()\n)")
                       ("keymap-set" "(keymap-set '$0 \"\" #')")
                       ("lambda" "(lambda ($0)\n)")
                       ("message" "(message $0)")))

(defun jacob-eval-print-last-sexp ()
  "Run `eval-print-last-sexp', indent the result."
  (interactive)
  (save-excursion
    (eval-print-last-sexp 0))
  (save-excursion
    (forward-line)
    (indent-pp-sexp t)))

(setopt elisp-flymake-byte-compile-load-path load-path)

(keymap-set lisp-interaction-mode-map "C-j" #'jacob-eval-print-last-sexp)

(jacob-require 'mermaid-mode)

(jacob-require 'ob-mermaid)

(require 'org)

(defun jacob-org-babel-tangle-delete-whitespace ()
  "Get rid of the whitespace at the end of the buffer."
  (goto-char (point-max))
  (delete-trailing-whitespace)
  (backward-delete-char 1)
  (save-buffer))

(add-hook 'org-babel-post-tangle-hook 'jacob-org-babel-tangle-delete-whitespace)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((octave . t)
   (sql . t)
   (js . t)
   (mermaid . t)))

;; stolen from doom
(with-no-warnings
  (custom-declare-face '+org-todo-active  '((t (:inherit (bold font-lock-constant-face org-todo)))) "")
  (custom-declare-face '+org-todo-project '((t (:inherit (bold font-lock-doc-face org-todo)))) "")
  (custom-declare-face '+org-todo-onhold  '((t (:inherit (bold warning org-todo)))) "")
  (custom-declare-face '+org-todo-cancel  '((t (:inherit (bold font-lock-comment-face org-todo)))) ""))

(setopt org-startup-folded t
        org-tags-column 0
        org-capture-templates '(("i" "Inbox" entry (file "") "* TODO %?\n:PROPERTIES:\n:CREATED: %U\n:END:"))
        org-todo-keywords '((sequence
                             "TODO(t)"  ; A task that needs doing & is ready to do
                             "PROJ(p)"  ; A project, which usually contains other tasks
                             "LOOP(r)"  ; A recurring task
                             "STRT(s)"  ; A task that is in progress
                             "WAIT(w)"  ; Something external is holding up this task
                             "HOLD(h)"  ; This task is paused/on hold because of me
                             "IDEA(i)"  ; An unconfirmed and unapproved task or notion
                             "|"
                             "DONE(d)"  ; Task successfully completed
                             "KILL(k)") ; Task was cancelled, aborted, or is no longer applicable
                            (sequence
                             "[ ](T)"   ; A task that needs doing
                             "[-](S)"   ; Task is in progress
                             "[?](W)"   ; Task is being held up or paused
                             "|"
                             "[X](D)")  ; Task was completed
                            (sequence
                             "|"
                             "OKAY(o)"
                             "YES(y)"
                             "NO(n)"))
        org-todo-keyword-faces '(("[-]"  . +org-todo-active)
                                 ("STRT" . +org-todo-active)
                                 ("[?]"  . +org-todo-onhold)
                                 ("WAIT" . +org-todo-onhold)
                                 ("HOLD" . +org-todo-onhold)
                                 ("PROJ" . +org-todo-project)
                                 ("NO"   . +org-todo-cancel)
                                 ("KILL" . +org-todo-cancel)))

(require 'org-agenda)

(setopt org-agenda-skip-scheduled-if-done t
        org-agenda-skip-deadline-if-done t
        org-agenda-custom-commands '(("r" "Routine" agenda "" ((org-agenda-tag-filter-preset '("+tickler"))
                                                               (org-agenda-span 'day)))
                                     ("w" "Work" todo "" ((org-agenda-tag-filter-preset '("+work"))))
                                     ("j" "Jobs"
                                      agenda ""
                                      ((org-agenda-span 3)
                                       (org-agenda-start-day "-1d")
                                       (org-agenda-time-grid '((daily today require-timed)
                                                               nil
                                                               " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄"))
                                       (org-agenda-tag-filter-preset '("-tickler" "-work"))))))

(defvar-keymap jacob-org-agenda-map
  "a" #'org-agenda
  "c" #'org-capture)

(keymap-set jacob-xfk-map "a" `("Agenda" . ,jacob-org-agenda-map))

(jacob-defhookf org-agenda-mode-hook
  (jacob-xfk-local-key "q" #'quit-window)
  (jacob-xfk-local-key "g" #'org-agenda-redo-all))

(jacob-require 'ob-mermaid)

(require 'org-src)

(setopt org-src-preserve-indentation t)

(require 'org-compat)
(setopt org-calendar-to-agenda-key nil  ; don't bind calendar key
        org-calendar-insert-diary-entry-key nil) ; don't bind calendar key

(require 'ox-latex)

(setopt org-latex-pdf-process (list "latexmk -pdf %f -shell-escape")) ; probably requires texlive

(require 'ox-extra)

(ox-extras-activate '(latex-header-blocks ignore-headlines))

(jacob-require 'org-edna)
(org-edna-mode 1)
(delight 'org-edna-mode nil t)

(require 'pulse)

(defun jacob-pulse-line (&rest _)
  "Pulse the current line."
  (pulse-momentary-highlight-region (save-excursion
                                      (back-to-indentation)
                                      (point))
                                    (line-end-position)))

(dolist (command '(recenter-top-bottom
                   scroll-up-command
                   scroll-down-command
                   other-window
                   xref-find-definitions
                   xref-pop-marker-stack
                   isearch-done))
  (advice-add command :after #'jacob-pulse-line))

(defun jacob-pulse-defun (&rest _)
  "Pulse the defun at point."
  (let ((bounds (bounds-of-thing-at-point 'defun)))
    (pulse-momentary-highlight-region (car bounds) (cdr bounds))))

(advice-add #'eval-defun :after #'jacob-pulse-defun)

(require 'server)
(server-start)

(require 'smerge-mode)
(defvar-keymap jacob-smerge-repeat-map
  :repeat t
  "l" #'smerge-next
  "j" #'smerge-prev
  "i" #'smerge-keep-upper
  "k" #'smerge-keep-lower
  "SPC" #'smerge-keep-all)

(require 'calendar)

(jacob-defhookf calendar-mode-hook
  (jacob-xfk-local-key "q" 'quit-window)
  (jacob-xfk-local-key "i" 'calendar-backward-week)
  (jacob-xfk-local-key "k" 'calendar-forward-week)
  (jacob-xfk-local-key "j" 'calendar-backward-day)
  (jacob-xfk-local-key "l" 'calendar-forward-day)
  (jacob-xfk-local-key "u" 'calendar-backward-month)
  (jacob-xfk-local-key "o" 'calendar-forward-month)
  (jacob-xfk-local-key "d" 'diary-view-entries)
  (jacob-xfk-local-key "s" 'diary-insert-entry)
  (jacob-xfk-local-key "m" 'diary-mark-entries)
  (jacob-xfk-local-key "." 'calendar-goto-today)
  (jacob-xfk-local-key "t" 'calendar-set-mark))

(add-hook 'calendar-today-visible-hook 'calendar-mark-today)

(setopt calendar-date-style 'european
        calendar-date-display-form '((if dayname
                                         (concat dayname ", "))
                                     day "/" month "/" year)
        calendar-week-start-day 1
        calendar-mark-holidays-flag t)

;; indent
;; make tab key call indent command or insert tab character, depending on cursor position
(setq-default tab-always-indent 'complete)

(require 'grep)
(when jacob-is-windows
  (setopt find-program "C:/Program Files (x86)/GnuWin32/bin/find.exe"))

(require 'winner)
(winner-mode 1)
(keymap-set xah-fly-command-map "1" #'winner-undo)
(keymap-set xah-fly-command-map "2" #'winner-redo)

(require 'compile)

(jacob-defhookf compilation-mode-hook
  (jacob-xfk-local-key "g" #'recompile))

(jacob-defhookf compilation-filter-hook
  (ansi-color-compilation-filter))

(setopt compilation-always-kill t
        compilation-scroll-output t)

(keymap-global-set "<f5>" 'recompile)

(require 'sql)

(defun jacob-sql-connect ()
  "Wrapper for `sql-connect' to set postgres password.
CONNECTION is the connection settings."
  (interactive)
  (let ((connection (sql-read-connection "Connection: ")))
    (with-environment-variables
        (("PGPASSWORD" (cadr (assoc 'sql-password
                                    (assoc-string connection
                                                  sql-connection-alist
                                                  t)))))
      (sql-connect connection))))

(jacob-defhookf sql-interactive-mode-hook
  (when (eq sql-product 'postgres)
    (setq sql-prompt-regexp "^[-[:alnum:]_]*[-=]\\*?[#>] ")
    (setq sql-prompt-cont-regexp "^\\(?:\\sw\\|\\s_\\)*[-(]\\*?[#>] "))
  (jacob-xfk-local-key "SPC , d" #'sql-send-paragraph))

(defun jacob-sqli-end-of-buffer ()
  "Move point to end of sqli buffer before sending paragraph.

Intended as before advice for `sql-send-paragraph'."
  (with-current-buffer sql-buffer
    (goto-char (point-max))))

(advice-add #'sql-send-paragraph :before #'jacob-sqli-end-of-buffer)

(keymap-set jacob-xfk-map "s" #'jacob-sql-connect)

(jacob-require 'pg "https://github.com/emarsden/pg-el")
(jacob-require 'pgmacs "https://github.com/emarsden/pgmacs")
(require 'pgmacs)

(require 'doc-view)
(jacob-defhookf doc-view-mode-hook
  (auto-revert-mode 1)
  (jacob-xfk-local-key "l" 'doc-view-next-page)
  (jacob-xfk-local-key "j" 'doc-view-previous-page))

(jacob-require 'pdf-tools)
(pdf-tools-install)

(require 'treesit)
(setopt treesit-language-source-alist '((c-sharp "https://github.com/tree-sitter/tree-sitter-c-sharp" "master" "src")
                                        (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
                                        (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
                                        (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
                                        (yaml "https://github.com/ikatyang/tree-sitter-yaml")
                                        (gdscript "https://github.com/PrestonKnopp/tree-sitter-gdscript"))
        treesit-load-name-override-list '((c-sharp "libtree-sitter-csharp" "tree_sitter_c_sharp"))
        major-mode-remap-alist '((csharp-mode . csharp-ts-mode))
        treesit-font-lock-level 4)      ; max level of fontification

(require 'yaml-ts-mode)

(add-to-list 'auto-mode-alist '("\\.ya?ml\\'" . yaml-ts-mode))

(require 'typescript-ts-mode)
;; In emacs core there are treesitter modes for js, jsx, ts and
;; tsx. Why not use tsx mode for all of them? Only one mode to
;; configure that way.

(add-to-list 'auto-mode-alist '("\\.[jt]s[xm]?\\'" . tsx-ts-mode))

(require 'message)
(jacob-defhookf message-mode-hook
  (setq-local auto-save-visited-mode nil))
(setopt message-send-mail-function 'smtpmail-send-it)

(require 'gnus)
(jacob-defhookf gnus-started-hook
  (gnus-demon-add-handler 'gnus-demon-scan-news 2 t))

(setopt gnus-use-full-window t
        gnus-always-read-dribble-file t)

(keymap-set jacob-xfk-map "g" #'gnus)

(require 'gnus-group)
(jacob-defhookf gnus-group-mode-hook
  (jacob-xfk-local-key "q" #'gnus-group-exit)
  (jacob-xfk-local-key "i" #'gnus-group-prev-group)
  (jacob-xfk-local-key "k" #'gnus-group-next-group)
  (jacob-xfk-local-key "g" #'gnus-group-get-new-news))

(require 'gnus-notifications)
(add-hook 'gnus-after-getting-new-news-hook 'gnus-notifications)

(require 'gnus-sum)
(jacob-defhookf gnus-summary-mode-hook
  (jacob-xfk-local-key "q" #'gnus-summary-exit)
  (jacob-xfk-local-key "i" #'gnus-summary-prev-article)
  (jacob-xfk-local-key "k" #'gnus-summary-next-article)
  (jacob-xfk-local-key "j" #'gnus-summary-prev-page)
  (jacob-xfk-local-key "l" #'gnus-summary-next-page))

(require 'gnus-topic)
(add-hook 'gnus-group-mode-hook 'gnus-topic-mode)

(defun jacob-gnus-topic-mode-hook-function ()
  "Hook function to be used with `gnus-topic-mode-hook'."
  (jacob-xfk-local-key "s" #'gnus-topic-select-group))

(require 'nxml-mode)
(add-to-list 'auto-mode-alist '("Directory.Packages.props" . nxml-mode))

(jacob-require 'key-chord)
(key-chord-mode 1)

(jacob-require 'avy)

(defun jacob-avy-action-xref (pt)
  "Call `xref-find-definitions' at PT."
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

(jacob-require 'apheleia)

(apheleia-global-mode 1)
(setq-default apheleia-inhibit t) ; set `apheleia-inhibit' to nil to enable
(add-to-list 'apheleia-formatters '(csharpier "dotnet" "csharpier" "--write-stdout"))
(add-to-list 'apheleia-mode-alist '(csharp-ts-mode . csharpier))

(defun jacob-apheleia-skip-function ()
  "Function for `apheleia-skip-functions'.
If point is in a yasnippet field or the minibuffer or region are
active, do not format the buffer."
  (or (seq-find (lambda (overlay)
                  (overlay-get overlay 'yas--snippet))
                (overlays-at (point)))
      (minibuffer-window-active-p (car (window-list)))
      (region-active-p)))

(add-to-list 'apheleia-skip-functions #'jacob-apheleia-skip-function)

(jacob-require 'rainbow-mode)
(add-hook 'prog-mode-hook #'rainbow-mode)
(delight 'rainbow-mode)

(jacob-require 'eglot-booster "https://github.com/jdtsmith/eglot-booster")
(eglot-booster-mode 1)

(jacob-require 'dape)

(setopt dape-info-hide-mode-line nil
        dape-buffer-window-arrangement 'right)

(add-to-list 'dape-configs '(netcoredbg-attach-port
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
                                 (cdr (assoc selection collection))))))

(remove-hook 'dape-start-hook #'dape-info)

(jacob-require 'csharp-toolbox "https://github.com/lem102/csharp-toolbox.git") ; TODO: can i make this use ssh?

;; (keymap-set jacob-xfk-map "c f" #'csharp-toolbox-format-statement)
;; (keymap-set jacob-xfk-map "c t" #'csharp-toolbox-run-test)
;; (keymap-set jacob-xfk-map "c a" #'csharp-toolbox-toggle-async)
;; (keymap-set jacob-xfk-map "c n" #'csharp-toolbox-guess-namespace)
;; (keymap-set jacob-xfk-map "c ;" #'csharp-toolbox-wd40)

(require 'switch-window)
(setopt switch-window-shortcut-style 'qwerty
        switch-window-threshold 3)
(keymap-set xah-fly-command-map "," #'switch-window)

(require 'tex)
(jacob-ensure-installed 'auctex)

(add-to-list 'auto-mode-alist '("\\.tex\\'" . latex-mode))

(setopt TeX-auto-save t
        TeX-parse-self t
        japanese-TeX-error-messages nil)

(TeX-global-PDF-mode 1)

(jacob-require 'markdown-mode)

(jacob-require 'feature-mode)

(jacob-require 'vertico)
(vertico-mode 1)

(require 'vertico-mouse)
(vertico-mouse-mode 1)

(jacob-require 'orderless)
(setopt completion-styles '(orderless initials))

(jacob-require 'marginalia)
(marginalia-mode 1)

(jacob-require 'consult)
(defun jacob-project-search ()
  "Wrapper for grep commands."
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
      (funcall orig-state action (funcall filter action candidate)))))

(setopt completion-in-region-function 'consult-completion-in-region
        xref-show-xrefs-function 'consult-xref
        xref-show-definitions-function 'consult-xref
        consult--source-buffer (plist-put consult--source-buffer
                                          :state #'jacob-consult-buffer-state-no-tramp))

(keymap-set xah-fly-leader-key-map "v" #'consult-yank-from-kill-ring)
(keymap-set xah-fly-leader-key-map "f" #'consult-buffer)
(keymap-set xah-fly-leader-key-map "i j" #'consult-recent-file)
(keymap-set xah-fly-leader-key-map "e s" #'consult-line)
(keymap-set xah-fly-leader-key-map "k u" #'consult-goto-line)

(keymap-set project-prefix-map "g" #'jacob-project-search)

(require 'consult-imenu)
(keymap-global-set "M-g i" #'consult-imenu)

(jacob-require 'embark)

(setopt embark-cycle-key "\\"
        embark-help-key "h")

(keymap-set xah-fly-command-map "\\" #'embark-act)

(jacob-require 'expand-region)
(setopt expand-region-contract-fast-key "9")
(keymap-set xah-fly-command-map "8" #'er/expand-region)

(jacob-require 'verb)
(add-hook 'org-mode-hook 'verb-mode)
(jacob-defhookf verb-response-body-mode-hook
  (jacob-xfk-local-key "q" #'quit-window))

(defun jacob-verb-id (response-id)
  "Get the id property from the stored verb response pertaining to RESPONSE-ID."
  (verb-json-get (oref (verb-stored-response response-id) body) "id"))

(jacob-require 'sly)

(sly-setup)

(sly-symbol-completion-mode 0)

(jacob-defhookf sly-mode-hook
  (jacob-xfk-local-key "SPC , m" #'sly-eval-last-expression)
  (jacob-xfk-local-key "SPC , d" #'sly-compile-defun)
  (jacob-xfk-local-key "SPC , e" #'sly-eval-buffer)
  (jacob-xfk-local-key "SPC w k" #'sly-edit-definition)

  (unless (sly-connected-p)
    (save-excursion
      (sly))))

(jacob-defhookf sly-db-hook
  (jacob-xfk-local-key "q" #'sly-db-quit))

(jacob-require 'sly-overlay)

(jacob-require 'sly-macrostep)

(jacob-require 'sly-stepper "https://github.com/joaotavora/sly-stepper.git")

(jacob-require 'sly-quicklisp)

(jacob-require 'sql-indent)

(jacob-require 'gptel)

(jacob-require 'gdscript-mode)

(push '(gdscript-mode "localhost" 6008) eglot-server-programs)

;; (jacob-require 'slack)

;; (defun jacob-slack-modeline-formatter (alist)
;;   "Hide the slack modeline if there are no notifications.

;; Element in ALIST is ((team-name . ((thread . (has-unreads
;; . mention-count)) (channel . (has-unreads . mention-count)))))"
;;   (if (seq-find (lambda (team)
;;                   (seq-find (lambda (room-type)
;;                               (car (cdr room-type)))
;;                             (cdr team)))
;;                 alist)
;;       (slack-default-modeline-formatter alist)
;;     ""))

;; (defun jacob-slack-show-unread ()
;;   "Open an unread slack message."
;;   (interactive)
;;   (let* ((team (slack-team-select))
;;          (rooms (seq-filter #'(lambda (room)
;;                                 (slack-room-has-unread-p room team))
;;                             (append (slack-team-ims team)
;;                                     (slack-team-groups team)
;;                                     (slack-team-channels team)))))
;;     (if (null rooms)
;;         (message "no unread slack messages")
;;       (slack-room-display (seq-first rooms) team))))

;; (setopt slack-enable-global-mode-string t
;;         slack-buffer-emojify t
;;         slack-prefer-current-team t
;;         slack-thread-also-send-to-room nil
;;         slack-modeline-formatter #'jacob-slack-modeline-formatter)

;; (setopt alert-default-style 'notifications)

;; (setopt lui-fill-type nil
;;         lui-time-stamp-position 0
;;         lui-time-stamp-format "%a %b %e %H:%M")

;; (defun jacob-slack-hook-function ()
;;   "Function to be run in slack mode hooks."
;;   (toggle-word-wrap 1))

;; (add-hook 'slack-message-buffer-mode-hook 'jacob-slack-hook-function)
;; (add-hook 'slack-thread-message-buffer-mode-hook 'jacob-slack-hook-function)

;; (add-to-list 'display-buffer-alist '((or (derived-mode . slack-mode)
;;                                          (derived-mode . lui-mode))
;;                                      (display-buffer-in-side-window)
;;                                      (side . right)))

;; (defun jacob-consult-slack-filter ()
;;   "Filter for slack buffers."
;;   (consult--buffer-query :sort 'visibility
;;                          :as #'buffer-name
;;                          :include "^*slack"))

;; (defvar jacob-consult-slack-source
;;   `( :name     "Slack"
;;      :narrow   ?s
;;      :category buffer
;;      :face     consult-buffer
;;      :history  buffer-name-history
;;      :items    ,#'jacob-consult-slack-filter
;;      :action   ,#'switch-to-buffer))

;; (add-to-list 'consult-buffer-sources jacob-consult-slack-source "APPEND")


;; personal functions

(defun jacob-indent-buffer ()
  "Indent whole buffer.  Designed for use in `before-save-hook'."
  (unless (ignore-errors smerge-mode)
    (indent-region (point-min) (point-max))))

(define-minor-mode jacob-screen-sharing-mode
  "Minor mode for sharing screens."
  :global t
  :group 'jacob
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
           (format-position (cdr jacob-format-words-style-and-start)))
      (goto-char format-position)
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

(defun jacob-github-push-set-upstream ()
  "Push current git branch to new upstream branch.
Try go to the create a pull request page if possible."
  (interactive)
  (with-temp-buffer
    (shell-command "git push --set-upstream origin HEAD" t)
    (when (search-forward "http" nil "NOERROR")
      (browse-url-at-point))))

(defvar jacob-gitlab-push-set-upstream-jira-url ""
  "URL for current employer's jira board.")

(defun jacob-gitlab-push-set-upstream ()
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
         (jira-link (concat jacob-gitlab-push-set-upstream-jira-url mr-key))
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

(provide 'init)

;;; init.el ends here
