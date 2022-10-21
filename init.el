;;; init.el --- Jacob's main init file. -*-lexical-binding: t-*-
;;; Commentary:
;;; Code:

(defvar jacob-emacs-mode (cond ((member "--planner" command-line-args)
                                (setq command-line-args (delete "--planner" command-line-args))
                                (setq frame-title-format "Emacs Planner")
                                'planner)
                               (t
                                'master))
  "The mode of this Emacs.")

(defmacro jacob-is-emacs-mode (mode &rest body)
  "If MODE is the current Emacs mode, evaluate BODY."
  (declare (indent 1))
  `(when (eq jacob-emacs-mode ,mode)
     ,@body))

(jacob-is-emacs-mode 'master
  (defun jacob-open-planner ()
    "Start a new emacs process configured to be a planner."
    (interactive)
    (start-process "Emacs Planner" nil "emacs" "--planner")))



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

(defvar jacob-raspberry-pi-ip-address
  nil "IP address of rasperry pi.")

(defvar jacob-omnisharp-language-server-path
  nil "Location of the omnisharp executable/start script.")

(defvar jacob-font-size
  12 "Font size to use.")

(defvar jacob-camunda-modeler-executable
  nil "Full path to camunda modeler executable.")

(when (file-exists-p "~/.emacs.d/environment.el")
  (load-file "~/.emacs.d/environment.el"))


;; mouse config

(setq scroll-conservatively 100)
(setq mouse-wheel-progressive-speed nil)
(setq mouse-wheel-scroll-amount '(10
                                  ((shift)
                                   . hscroll)
                                  ((meta))
                                  ((control)
                                   . text-scale)))


;; user interface config

(setq-default use-dialog-box nil)
(setq use-short-answers t)
(setq ring-bell-function 'ignore)
(setq-default truncate-lines nil)
(setq confirm-kill-processes nil)
(setq switch-to-buffer-obey-display-actions t)
(setq disabled-command-function nil)
(setq enable-recursive-minibuffers t)

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
                                 "\"An idiot admires complexity, a genius admires simplicity.\" - Terry A. Davis"
                                 "\"Perfection is achieved, not when there is nothing more to add, but when there is nothing left to take away.\" - Antoine de Saint-Exupéry")
  "List of messages to display in scratch buffer.")

(setq initial-scratch-message (concat ";; " (nth (random (length jacob-welcome-messages)) jacob-welcome-messages) "\n\n"))

(setq display-time-default-load-average nil)
(setq display-time-day-and-date t)

;; (setq tab-bar-format '(tab-bar-format-global))
;; (tab-bar-mode 1)

(setq display-time-format "%H:%M %d/%m/%Y")
(display-time-mode 1)


;; bookmark config

(setq bookmark-set-fringe-mark nil)


;; unicode

(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)


;; help at point

(setq-default help-at-pt-display-when-idle '(flymake-diagnostic))
(help-at-pt-set-timer)


;; editing config

(global-subword-mode 1)

(setq show-paren-style 'parenthesis)
(setq show-paren-when-point-inside-paren t)
(setq show-paren-when-point-in-periphery nil)
(show-paren-mode 1)

(setq electric-pair-preserve-balance t)
(setq electric-pair-delete-adjacent-pairs t)
(setq electric-pair-open-newline-between-pairs t)
(electric-pair-mode 1)

(delete-selection-mode 1)

(repeat-mode 1)

(setq dabbrev-case-fold-search nil)
(setq dabbrev-case-replace nil)

(setq line-move-visual t)


;; mode line config

(column-number-mode 1)
(line-number-mode 1)

(defvar jacob-mode-line-format
  '(" %e"
    mode-line-front-space
    "%*"
    (:eval (pcase major-mode
             ('lisp-interaction-mode "ELi")
             ('emacs-lisp-mode "EL")
             ('typescript-react-mode "TSX")
             ('nxml-mode "XML")
             (_ mode-name)))
    ": "
    "%b "
    (vc-mode vc-mode)
    " "
    mode-line-position
    (xah-fly-keys " ∑")
    (flymake-mode flymake-mode-line-format)
    " "
    (eglot--managed-mode
     (" " eglot--mode-line-format " "))
    (global-mode-string
     ("" global-mode-string)))
  "Custom mode line format.")

(setq-default mode-line-format jacob-mode-line-format)


;; tramp

(with-eval-after-load 'tramp
  (defvar jacob-raspberry-pi-connection-string
    (concat "/" tramp-default-method ":pi@" jacob-raspberry-pi-ip-address ":")
    "Raspberry Pi connection string for tramp."))

;; tell vc to ignore tramp files
(setq vc-ignore-dir-regexp
      (format "\\(%s\\)\\|\\(%s\\)"
              vc-ignore-dir-regexp
              tramp-file-name-regexp))

;; lots of problems. for now, disable it!
(setq tramp-archive-enabled nil)


;; theme config

(when (display-graphic-p)
  (set-face-attribute 'default nil :background "honeydew3")
  (set-face-attribute 'fringe nil :background "honeydew3")
  (set-face-attribute 'mode-line nil :background "limegreen")
  (set-face-attribute 'mode-line-inactive nil :background "lightgreen"))


;; js-mode config

(put 'js-indent-level 'safe-local-variable #'numberp)
(setq js-indent-level 2)


;; cc-mode config

(setq-default c-basic-offset 4)


;; dired-mode config

(with-eval-after-load 'dired
  (setq dired-recursive-copies 'always)
  (setq dired-recursive-deletes 'top)
  (setq dired-dwim-target t)
  (setq delete-by-moving-to-trash t)
  (setq dired-kill-when-opening-new-dired-buffer t)

  (require 'ls-lisp)
  (setq ls-lisp-use-insert-directory-program nil)
  (setq ls-lisp-dirs-first t)

  (defun jacob-dired-mode-setup ()
    "Hook function for dired."
    (require 'dired-x)
    (setq dired-guess-shell-alist-user '(("\\.mkv\\'" "mpv")))
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
      (save-excursion
        (set-buffer (get-buffer "*Eshell Async Command Output*"))
        (rename-buffer buffer-name))))

  (defun pcomplete/gco ()
    (pcomplete-here* (jacob-git-get-branches)))

  (defun pcomplete/grh ()
    (pcomplete-here* (jacob-git-get-branches t)))

  (defun eshell/gpsu ()
    (let* ((create-mr (y-or-n-p "Create MR on GitLab?"))
           (command (concat "git push --set-upstream origin HEAD "
                            (when create-mr (let* ((branch-name (with-temp-buffer
                                                                  (eshell-command "git symbolic-ref HEAD --short" t)
                                                                  (buffer-substring-no-properties (point-min) (- (point-max) 1))))
                                                   (mr-key (progn
                                                             (string-match (rx "mer"  "-" (+ digit))
                                                                           branch-name)
                                                             (match-string 0 branch-name))))
                                              (concat "-o merge_request.create "
                                                      "-o merge_request.remove_source_branch "
                                                      (concat "-o merge_request.description=\""
                                                              "[" mr-key "](" "https://coveaprodcloud.atlassian.net/browse/" mr-key ")"
                                                              "\"")))))))
      (with-temp-buffer
        (eshell-command command t)
        (goto-char (point-min))
        (search-forward "https")
        (browse-url-at-point))))

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


;; emacs-lisp-mode config

(defun jacob-elisp-config-hook-function ()
  "Configure `emacs-lisp-mode' when hook run."
  (flymake-mode 1)
  (eldoc-mode 1))

(add-hook 'emacs-lisp-mode-hook 'jacob-elisp-config-hook-function)


;; font config

(defvar jacob-font-name
  (cdr (assoc-string system-type '(("windows-nt" . "Consolas")
                                   ("darwin" . "Menlo")
                                   ("gnu/linux" . "DejaVu Sans Mono")))))

(defun jacob-set-font-size (size)
  "Set font to SIZE."
  (set-frame-font (concat jacob-font-name "-" (number-to-string size)) nil t)
  (setq jacob-font-size size))

(defun jacob-font-size-increase ()
  "Increase font size by two steps."
  (interactive)
  (jacob-set-font-size (+ jacob-font-size 2)))

(defun jacob-font-size-decrease ()
  "Decrease font size by two steps."
  (interactive)
  (jacob-set-font-size (- jacob-font-size 2)))

(jacob-set-font-size jacob-font-size)

(defvar jacob-font-size-repeat-map
  (let ((map (make-sparse-keymap)))
    (define-key map "a" 'jacob-font-size-increase)
    (define-key map "s" 'jacob-font-size-decrease)
    map))

(put 'jacob-font-size-increase 'repeat-map 'jacob-font-size-repeat-map)
(put 'jacob-font-size-decrease 'repeat-map 'jacob-font-size-repeat-map)

;; enable emoji fonts
(set-fontset-font t
                  '(#x1f300 . #x1fad0)
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

(with-eval-after-load 'org-mode
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
   '((octave . t)))

  (setq org-confirm-babel-evaluate nil))


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
                   ))
  (advice-add command :after #'jacob-pulse-line))


;; server config

(when (equal jacob-emacs-mode 'master)
  (server-start))


;; time emacs startup

(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs ready in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))


;; calendar + diary config

(when (and (boundp 'jacob-raspberry-pi-ip-address)
           (boundp 'jacob-raspberry-pi-connection-string))
  (setq diary-file (concat jacob-raspberry-pi-connection-string
                           "/home/pi/org/jacobsDiary.diary")))

(with-eval-after-load 'calendar
  (setq diary-date-forms diary-european-date-forms)
  (setq calendar-date-style 'european)
  (setq calendar-date-display-form '((format "%02d/%02d/%04d" (string-to-number day) (string-to-number month) (string-to-number year))))
  (setq calendar-week-start-day 1)
  (setq calendar-mark-diary-entries-flag t)
  (setq calendar-mark-holidays-flag t)
  (add-hook 'calendar-today-visible-hook 'calendar-mark-today))

(jacob-is-emacs-mode 'planner
  (defun jacob-launch-dashboard-when-idle ()
    "Launch informative dashboard after idle time."
    (run-with-idle-timer 1 nil (lambda ()
                                 (calendar)
                                 (diary-view-entries)
                                 (other-window 1)
                                 (split-window-horizontally)
                                 (other-window 1)
                                 (find-file (concat jacob-raspberry-pi-connection-string "/home/pi/org/todo.org")))))
  (add-hook 'after-init-hook 'jacob-launch-dashboard-when-idle))


;; remember config

(with-eval-after-load 'remember
  (setq remember-data-file (concat jacob-raspberry-pi-connection-string "/home/pi/org/remember")))


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
    (cond ((seq-contains-p '(csharp-tree-sitter-mode
                             typescript-react-mode
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

  (jacob-is-emacs-mode 'master
    (add-hook 'after-init-hook (lambda ()
                                 ;; maximize window
                                 (w32-send-sys-command 61488))))

  (defun jacob-confirm-terminate-batch-job ()
    "Type y and enter to terminate batch job after sending ^C."
    (when (not (null eshell-process-list))
      (insert "y")
      (eshell-send-input)))

  (advice-add 'eshell-interrupt-process :after #'jacob-confirm-terminate-batch-job))


;; winner-mode

(winner-mode 1)


;; docview config

(with-eval-after-load 'doc-view-mode
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



;; package installation

(require 'package)

(defmacro jacob-is-installed (package &rest body)
  "If PACKAGE is installed, evaluate BODY.
  Used when attempting to lazy load PACKAGE."
  (declare (indent 1))
  `(when (package-installed-p ,package)
     ,@body))

(defmacro jacob-try-require (feature &rest body)
  "Attempt to require FEATURE.
  If successful, evaluate BODY.
  Used to eagerly load feature."
  (declare (indent 1))
  `(when (require ,feature nil 'noerror)
     ,@body))

(setq package-archives '(
                         ("GNU" . "https://elpa.gnu.org/packages/")
                         ("non-GNU" . "https://elpa.nongnu.org/nongnu/")
                         ("melpa" . "https://melpa.org/packages/")
                         ))

(defvar jacob-packages '(
                         ;; essential
                         xah-fly-keys
                         expand-region
                         ;; major modes
                         ahk-mode
                         csharp-mode
                         go-mode
                         sml-mode
                         json-mode
                         csv-mode
                         powershell
                         yaml-mode
                         markdown-mode
                         typescript-mode
                         racket-mode
                         feature-mode
                         fsharp-mode
                         purescript-mode
                         dotenv-mode
                         restclient
                         dockerfile-mode
                         kotlin-mode
                         gdscript-mode
                         clojure-mode
                         dart-mode
                         ;; completion enhancements
                         vertico
                         consult
                         orderless
                         marginalia
                         ;; tree sitter
                         tree-sitter-langs
                         tree-sitter-indent
                         ;; programming
                         eglot
                         eglot-fsharp
                         lsp-mode
                         inf-ruby
                         cider
                         ;; misc
                         restart-emacs
                         docker-tramp)
  "List of packages to install.")

(defun jacob-wrangle-packages ()
  (interactive)
  (let* ((desired jacob-packages)
         (installed (mapcar #'car (package--alist)))
         (missing-packages (cl-set-difference desired installed))
         (unwanted-packages (cl-set-difference installed desired)))

    (when (not (null missing-packages))
      (package-refresh-contents))

    (mapc #'package-install
          missing-packages)

    (mapc (lambda (package-name)
            (ignore-errors
              (package-delete (package-get-descriptor package-name))))
          unwanted-packages)))



;; package configuration


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


;; csharp-mode

(jacob-is-installed 'csharp-mode
  (add-to-list 'auto-mode-alist '("\\.cs\\'" . csharp-tree-sitter-mode)))


;; eglot config

(jacob-is-installed 'eglot
  (setq eglot-confirm-server-initiated-edits nil)
  (load-file (expand-file-name "~/.emacs.d/myLisp/old-eglot-jdt.el"))
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
                (setq-local eldoc-documentation-strategy
                            'eldoc-documentation-compose)))

  (add-hook 'java-mode-hook 'eglot-ensure)
  ;; (add-hook 'csharp-tree-sitter-mode-hook 'eglot-ensure)
  (add-hook 'typescript-mode-hook 'eglot-ensure)
  (add-hook 'fsharp-mode-hook (lambda ()
                                (when (eq system-type 'gnu/linux)
                                  (require 'eglot-fsharp)
                                  (eglot-ensure))))
  (with-eval-after-load 'eglot
    (if (boundp 'jacob-omnisharp-language-server-path)
        (add-to-list 'eglot-server-programs `(csharp-tree-sitter-mode . (,jacob-omnisharp-language-server-path "-lsp"))))

    (add-to-list 'eglot-server-programs '((js-mode typescript-mode) . ("typescript-language-server" "--stdio")))

    ;; (add-to-list 'eglot-server-programs '((js-mode typescript-mode) . (eglot-deno "deno" "lsp")))

    (defclass eglot-deno (eglot-lsp-server) ()
      :documentation "A custom class for deno lsp.")

    (cl-defmethod eglot-initialization-options ((server eglot-deno))
      "Passes through required deno initialization options"
      (list :enable t
            :lint t))

    ;; (add-to-list 'eglot-server-programs '(go-mode . ("/home/jacob/go/bin/gopls")))

    (eglot--code-action eglot-code-action-organize-imports-ts "source.organizeImports.ts")
    (eglot--code-action eglot-code-action-add-missing-imports-ts "source.addMissingImports.ts")

    ))


;; lsp mode config, for csharp only

(jacob-is-installed 'lsp-mode
  (setq lsp-lens-enable nil)
  (setq lsp-headerline-breadcrumb-enable nil)
  (setq lsp-eldoc-render-all t)
  (setq lsp-eldoc-enable-hover t)

  (add-hook 'csharp-tree-sitter-mode-hook 'lsp)
  )



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


;; typescript config

(jacob-is-installed 'typescript-mode

  (put 'tsi-typescript-indent-offset 'safe-local-variable #'numberp)

  (define-derived-mode typescript-react-mode typescript-mode
    "TSX")

  (add-to-list 'auto-mode-alist '("\\.tsx?\\'" . typescript-react-mode))
  (with-eval-after-load 'tree-sitter
    (add-to-list 'tree-sitter-major-mode-language-alist '(typescript-react-mode . tsx))

    (jacob-try-require 'tsi
      (jacob-try-require 'tsi-typescript
        (add-hook 'typescript-react-mode-hook (lambda ()
                                                (tsi-typescript-mode 1)))))))


;; tree sitter config

(jacob-is-installed 'tree-sitter
  (add-hook 'typescript-react-mode-hook (lambda ()
                                          (global-tree-sitter-mode 1)
                                          (tree-sitter-hl-mode 1))))


;; xah-fly-keys config

(setq xah-fly-use-control-key nil)
(setq xah-fly-use-meta-key t)

(jacob-try-require 'xah-fly-keys
  (xah-fly-keys-set-layout "qwerty")
  (xah-fly-keys 1))



;; personal functions

(defun jacob-send-mr-message ()
  (interactive)
  (let* ((gitlab-url (read-from-minibuffer "gitlab-url: "))
         (ticket-details (let* ((gitlab-mr-api "https://gitlab.tools.digital.coveahosted.co.uk/api/v4/merge_requests")
                                (mrs (jacob-web-request-helper gitlab-mr-api
                                                               "GET"
                                                               '(("PRIVATE-TOKEN" . ""))
                                                               nil
                                                               nil
                                                               'json))
                                (target-mr (seq-find (lambda (mr)
                                                       (let-alist mr
                                                         (string= gitlab-url
                                                                  .web_url)))
                                                     mrs))
                                (mr-description (let-alist target-mr
                                                  .description)))
                           (string-match (rx "[" (group-n 1 (+ any)) "]"
                                             "(" (group-n 2 (+ any)) ")")
                                         mr-description)
                           (cons (match-string 1 mr-description)
                                 (match-string 2 mr-description))))
         (jira-ticket-name (car ticket-details))
         (jira-url (cdr ticket-details))
         (message (concat "Jacob Leeming: "
                          "<" gitlab-url "|MR> "
                          "for "
                          "<" jira-url "|" jira-ticket-name "> "
                          "ready for review.")))
    (jacob-web-request-helper ""
                              "POST"
                              nil
                              `((text . ,message))
                              'json-encode)))

(defun jacob-eval-print-last-sexp ()
  (interactive)
  (save-excursion
    (eval-print-last-sexp 0))
  (save-excursion
    (forward-line)
    (indent-pp-sexp t)))

(global-set-key (kbd "C-j") 'jacob-eval-print-last-sexp)

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

(defun jacob-web-request-helper (url &optional method headers data data-format-function data-parse)
  "Helper function for making web requests.
METHOD, HEADERS and DATA are for the corresponding url-request variables.
URL is the address to send the request to.
Returns a string containing the response.

DATA-FORMAT-FUNCTION is a function that takes one argument and returns
DATA in string form.

DATA-PARSE is a symbol specifying the output of this function. If not
given, it will return the http reponse in string form. If `json' it
will return the json data as a lisp object."
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

(defun jacob-open-in-camunda-modeler ()
  "Attempt to open current file in camunda modeler."
  (interactive)
  (start-process "camunda-modeler" nil jacob-camunda-modeler-executable buffer-file-name))

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
  "Wrapper command for `async-shell-command'."
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

(defun jacob-config-visit ()
  "Open the init file."
  (interactive)
  (find-file "~/.emacs.d/init.el"))

(defun jacob-config-reload ()
  "Evaluate the init file."
  (interactive)
  (load-file (expand-file-name "~/.emacs.d/init.el")))

(load-file (expand-file-name "~/.emacs.d/myLisp/jacob-long-time.el"))

(defun jacob-display-time ()
  "Display the current date and time in the echo area."
  (interactive)
  (message (concat (format-time-string "%A the %e")
                   (jacob-day-suffix (string-to-number (format-time-string "%e")))
                   (format-time-string " of %B, the year of our Lord %Y, ")
                   "at "
                   (jacob-long-time (string-to-number (format-time-string "%H"))
                                    (string-to-number (format-time-string "%M")))
                   ".")))

(defun jacob-insert-plus ()
  (interactive)
  (insert "+"))

(defun jacob-insert-equals ()
  (interactive)
  (insert "="))

(defun jacob-insert-apostrophe ()
  (interactive)
  (insert "'"))

(defun jacob-insert-at ()
  (interactive)
  (insert "@"))

(defun jacob-insert-tilde ()
  (interactive)
  (insert "~"))

(defun jacob-insert-hash ()
  (interactive)
  (insert "#"))

(defun jacob-insert-exclamation-mark ()
  (interactive)
  (insert "!"))

(defun jacob-insert-pound-sign ()
  (interactive)
  (insert "£"))

(defun jacob-insert-dollar-sign ()
  (interactive)
  (insert "$"))

(defun jacob-insert-percentage-sign ()
  (interactive)
  (insert "%"))

(defun jacob-insert-caret ()
  (interactive)
  (insert "^"))

(defun jacob-insert-ampersand ()
  (interactive)
  (insert "&"))

(defun jacob-insert-asterisk ()
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
  (seq-find (lambda (x)
              (string= x "package.json"))
            (directory-files (project-root (project-current)))))

(defun jacob-format-buffer-shell-command (command)
  (save-buffer)
  (shell-command (format command
                         (shell-quote-argument buffer-file-name)))
  (revert-buffer t t t))

(defun jacob-format-buffer ()
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

;; FIXME: keys that are not already bound will not work for jacob-xfk-define-key
(defun jacob-xfk-define-key (major-mode-keymap key command)
  "In MAJOR-MODE-KEYMAP bind KEY to COMMAND only when in xfk command mode."
  (define-key major-mode-keymap (vector 'remap (lookup-key xah-fly-command-map key)) command))

(defun jacob-git-push-set-upstream ()
  "Push current git branch to new upstream branch."
  (interactive)
  (shell-command "git push --set-upstream origin HEAD"))

(defun jacob-git-pull-master-new-branch ()
  "Push current git branch to new upstream branch."
  (interactive)
  (when (zerop (shell-command "git checkout master"))
    (when (zerop (shell-command "git pull"))
      (shell-command (concat "git checkout -b " (read-from-minibuffer "branch name: "))))))

(defun jacob-npm-fix-linting ()
  "Fix linting for npm then commit and push."
  (interactive)
  (when (zerop (shell-command "npm run lint:fix"))
    (shell-command "git commit -m \"chore: fix linting\" -na")
    (shell-command "git push")))

(defun jacob-open-in-vscode ()
  "Open current file in vscode."
  (interactive)
  (let ((default-directory (project-root (project-current)))
        (file (buffer-file-name))
        (line (number-to-string (+ (current-line) 1)))
        (column (number-to-string (+ (current-column) 1))))
    (shell-command (concat "code . --reuse-window --goto " file ":" line ":" column))))



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

(define-abbrev-table 'global-abbrev-table
  '(
    ("dal" "$")
    ;; ("eq" "=")
    ("eeq" "==")
    ("eeeq" "===")
    ("sco" "_")
    ))

(define-abbrev-table 'org-mode-abbrev-table
  '(("i" "I")))

(define-abbrev-table 'common-operators-abbrev-table
  '(
    ("lt" "<")
    ("gt" ">")
    ("lte" "<=")
    ("gte" ">=")
    ("eq" "==")
    ("neq" "!=")
    ("or" "||")
    ("and" "&&")
    ("ret" "return")))

(define-abbrev-table 'c-mode-abbrev-table
  '(
    ("if" "" jacob-insert-c-if)
    ("for" "" jacob-insert-c-for)
    ("while" "" jacob-insert-c-while)
    ("switch" "" jacob-insert-c-switch)
    ("case" "" jacob-insert-c-case))
  nil
  :parents (list common-operators-abbrev-table))

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
    ("it" "" jacob-insert-js-it))
  nil
  :parents (list c-mode-abbrev-table))

(define-abbrev-table 'typescript-mode-abbrev-table
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
  :parents (list c-mode-abbrev-table))

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
  :parents (list 'common-java-csharp-abbrev-table))

(define-abbrev-table 'csharp-tree-sitter-mode-abbrev-table
  '(("cwl" "" jacob-insert-csharp-print)
    ("as" "async")
    ("ns" "namespace")
    ("guid" "Guid")
    ("prop" "" jacob-insert-csharp-property))
  nil
  :parents (list 'common-java-csharp-abbrev-table))

(define-abbrev-table 'emacs-lisp-mode-abbrev-table
  '(("def" "" jacob-insert-elisp-defun)
    ("let" "" jacob-insert-lisp-let)
    ("int" "(interactive)" t)
    ("cond" "" jacob-insert-lisp-cond)
    ("gc" "" jacob-insert-elisp-goto-char)
    ("pmi" "(point-min)" t)
    ("pma" "(point-max)" t)
    ("weal" "" jacob-insert-elisp-with-eval-after-load)))

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



;; key bindings


;; macros

(fset 'jacob-return-macro [return])


;; xah-fly-keys keybindings

(jacob-is-installed 'xah-fly-keys
  (define-prefix-command 'jacob-config-keymap)
  (define-prefix-command 'jacob-eglot-keymap)

  (jacob-is-installed 'eglot
    (let ((map jacob-eglot-keymap))
      (define-key map "a" 'eglot-code-actions)
      (define-key map "r" 'eglot-rename)))

  (let ((map vc-prefix-map))
    (define-key map "P" 'jacob-git-push-set-upstream)
    (define-key map "c" 'jacob-git-pull-master-new-branch))

  (let ((map xah-fly-dot-keymap))
    (define-key map "v" vc-prefix-map)
    (define-key map "t" tab-prefix-map)
    (define-key map "c" jacob-config-keymap)
    (define-key map "p" project-prefix-map)
    (jacob-is-installed 'eglot
      (define-key map "e" jacob-eglot-keymap))
    (jacob-is-installed 'consult
      (define-key map "s" 'consult-line))
    (let ((map project-prefix-map))
      (define-key map "g" 'jacob-project-search))
    (define-key map "v" vc-prefix-map)
    (define-key map "b" 'modus-themes-toggle)
    (define-key map "i" 'jacob-format-buffer))

  (let ((map xah-fly-command-map))
    (define-key map "a" 'execute-extended-command)
    (define-key map "s" 'jacob-return-macro)
    (define-key map "DEL" nil)
    (define-key map "4" 'other-window-prefix)
    (define-key map "1" 'winner-undo)
    (define-key map "2" 'winner-redo)
    (define-key map "9" 'jacob-swap-visible-buffers)
    (define-key map "'" 'jacob-format-words)
    (jacob-is-installed 'expand-region
      (define-key map "8" 'er/expand-region)))

  (let ((map jacob-config-keymap))
    (define-key map "r" 'jacob-config-reload)
    (define-key map "e" 'jacob-config-visit)
    (define-key map "c" 'jacob-org-src-block)
    (define-key map "p" 'jacob-recompile-packages)
    (define-key map "t" 'jacob-display-time)
    (jacob-is-installed 'restart-emacs
      (define-key map "R" 'restart-emacs)))

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

  (let ((map xah-fly-e-keymap))
    (define-key map (char-to-string jacob-insert-parentheses-character) 'insert-pair)
    (define-key map (char-to-string jacob-insert-square-bracket-character) 'insert-pair)
    (define-key map (char-to-string jacob-insert-curly-brace-character) 'insert-pair)
    (define-key map (char-to-string jacob-insert-double-quote-character) 'insert-pair)
    (define-key map (char-to-string jacob-insert-single-quote-character) 'insert-pair)
    (define-key map (char-to-string jacob-insert-angle-bracket-character) 'insert-pair)
    (define-key map "m" 'xah-insert-hyphen)
    (define-key map "," 'xah-insert-low-line)
    (define-key map "." 'jacob-insert-equals)
    (define-key map "/" 'jacob-insert-plus)
    (define-key map "z" 'jacob-insert-apostrophe)
    (define-key map "x" 'jacob-insert-at)
    (define-key map "c" 'jacob-insert-hash)
    (define-key map (kbd "d") 'backward-delete-char)
    (define-key map "v" 'jacob-insert-tilde)
    (define-key map "e" 'jacob-insert-dollar-sign)
    (define-key map "r" 'jacob-insert-caret)
    (define-key map "o" 'jacob-insert-ampersand))

  (defvar jacob-recenter-repeat-map
    (let ((map (make-sparse-keymap)))
      (define-key map "p" 'recenter-top-bottom)
      map))

  (put 'recenter-top-bottom 'repeat-map 'jacob-recenter-repeat-map)

  (let ((map xah-fly-leader-key-map))
    (jacob-is-installed 'consult
      (define-key map "v" 'consult-yank-from-kill-ring)
      (define-key map "f" 'consult-buffer)))

  (let ((map xah-fly-w-keymap))
    (define-key map "n" 'jacob-eval-and-replace))

  (let ((map xah-fly-t-keymap))
    (define-key map "j" 'xah-close-current-buffer))

  (let ((map xah-fly-c-keymap))
    (define-key map "j" 'consult-recent-file)
    (define-key map "e" 'find-file))

  (let ((map xah-fly-t-keymap))
    (define-key map "j" 'kill-current-buffer))

  (let ((map xah-fly-r-keymap))
    (define-key map "c" 'kmacro-set-counter))

  (let ((map xah-fly-n-keymap))
    (define-key map "a" 'jacob-font-size-increase)
    (define-key map "3" 'jacob-async-shell-command))

  (let ((map vc-prefix-map))
    (define-key map "p" 'vc-push))

  (let ((map minibuffer-local-completion-map))
    (define-key map "SPC" 'self-insert-command))

  (let ((map dired-mode-map))
    (jacob-xfk-define-key map "q" 'quit-window)
    (jacob-xfk-define-key map "i" 'dired-previous-line)
    (jacob-xfk-define-key map "k" 'dired-next-line)
    (jacob-xfk-define-key map "s" 'dired-find-file)
    (jacob-xfk-define-key map "e" 'dired-mark)
    (jacob-xfk-define-key map "r" 'dired-unmark)
    (jacob-xfk-define-key map "x" 'dired-do-rename)
    (jacob-xfk-define-key map "c" 'dired-do-copy)
    (jacob-xfk-define-key map "d" 'dired-do-delete) ; we skip the "flag, delete" process as files are sent to system bin on deletion
    (jacob-xfk-define-key map "u" 'dired-up-directory)
    (jacob-xfk-define-key map "j" 'dired-goto-file))

  (let ((map occur-mode-map))
    (jacob-xfk-define-key map "q" 'quit-window)
    (jacob-xfk-define-key map "i" 'previous-error-no-select)
    (jacob-xfk-define-key map "k" 'next-error-no-select))

  (with-eval-after-load 'vc-dir
    (let ((map vc-dir-mode-map))
      (jacob-xfk-define-key map "q" 'quit-window)
      (jacob-xfk-define-key map "i" 'vc-dir-previous-line)
      (jacob-xfk-define-key map "k" 'vc-dir-next-line)
      (jacob-xfk-define-key map "o" 'vc-dir-next-directory)
      (jacob-xfk-define-key map "u" 'vc-dir-previous-directory)
      (jacob-xfk-define-key map "s" 'vc-dir-find-file)
      (jacob-xfk-define-key map "e" 'vc-dir-mark)
      (jacob-xfk-define-key map "r" 'vc-dir-unmark)
      (jacob-xfk-define-key map "v" 'vc-next-action)
      (jacob-xfk-define-key map "p" 'vc-push)
      (jacob-xfk-define-key map "P" 'jacob-git-push-set-upstream)))

  (with-eval-after-load 'info
    (let ((map Info-mode-map))
      (jacob-xfk-define-key map "q" 'quit-window)
      (jacob-xfk-define-key map "l" 'Info-scroll-up)
      (jacob-xfk-define-key map "j" 'Info-scroll-down)
      (jacob-xfk-define-key map "i" 'Info-up)
      (jacob-xfk-define-key map "k" 'Info-menu)))

  (with-eval-after-load 'calendar
    (let ((map calendar-mode-map))
      (jacob-xfk-define-key map "q" 'quit-window)
      (jacob-xfk-define-key map "i" 'calendar-backward-week)
      (jacob-xfk-define-key map "k" 'calendar-forward-week)
      (jacob-xfk-define-key map "j" 'calendar-backward-day)
      (jacob-xfk-define-key map "l" 'calendar-forward-day)
      (jacob-xfk-define-key map "u" 'calendar-backward-month)
      (jacob-xfk-define-key map "o" 'calendar-forward-month)
      (jacob-xfk-define-key map "d" 'diary-view-entries)
      (jacob-xfk-define-key map "s" 'diary-insert-entry)
      (jacob-xfk-define-key map "m" 'diary-mark-entries)
      (jacob-xfk-define-key map "." 'calendar-goto-today)
      (jacob-xfk-define-key map "t" 'calendar-set-mark)))

  (with-eval-after-load 'doc-view
    (let ((map doc-view-mode-map))
      (jacob-xfk-define-key map "l" 'doc-view-next-page)
      (jacob-xfk-define-key map "j" 'doc-view-previous-page)))

  (with-eval-after-load 'diff-mode
    (let ((map diff-mode-map))
      (jacob-xfk-define-key map "q" 'quit-window))))

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



(provide 'init)
;;; init.el ends here
