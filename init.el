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

(if (file-exists-p "~/.emacs.d/environment.el")
    (load-file "~/.emacs.d/environment.el"))


;; mouse config

(setq scroll-conservatively 100)
(setq mouse-wheel-progressive-speed nil)


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
  (if jacob-screen-sharing-mode
      (progn
        (global-hl-line-mode 1)
        (global-display-line-numbers-mode 1))
    (progn
      (global-hl-line-mode 0)
      (global-display-line-numbers-mode 0))))


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

(eval-and-compile
  (add-to-list 'load-path "~/.emacs.d/init/")
  (add-to-list 'load-path "~/.emacs.d/local-packages/"))

(setq read-process-output-max (* 1024 1024))

(setq custom-file (make-temp-file "emacs-custom-"))

(setq inhibit-startup-screen t)

(setq split-height-threshold nil)

(setq parens-require-spaces nil)

(defvar jacob-welcome-messages '("\"A journey of a thousand miles begins with a single step.\" - 老子"
                                 "\"apex predator of grug is complexity\" - some grug"
                                 "\"An idiot admires complexity, a genius admires simplicity.\" - Terry A. Davis")
  "List of messages to display in scratch buffer.")

(setq initial-scratch-message (concat ";; " (nth (random (length jacob-welcome-messages)) jacob-welcome-messages) "\n\n"))

(setq display-time-default-load-average nil)
(setq display-time-day-and-date t)

(setq tab-bar-format '(tab-bar-format-global))
(tab-bar-mode 1)

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


;; mode line

(column-number-mode 1)
(line-number-mode 1)

(defvar jacob-mode-line-format
  (list "%*" ; saved, readonly
        "%m: " ; major mode
        "%b " ; buffer name
        mode-line-position
        mode-line-misc-info ; for use with org timer
        )
  "Custom mode line format.")

(setq-default mode-line-format jacob-mode-line-format)


;; tramp

(with-eval-after-load 'tramp
  (defvar jacob-raspberry-pi-connection-string
    (concat "/" tramp-default-method ":pi@" jacob-raspberry-pi-ip-address ":")
    "Raspberry Pi connection string for tramp."))

(setq vc-ignore-dir-regexp
      (format "\\(%s\\)\\|\\(%s\\)"
              vc-ignore-dir-regexp
              tramp-file-name-regexp))

;; lots of problems. for now, disable it!
(setq tramp-archive-enabled nil)


;; theme config

(set-face-attribute 'default nil :background "honeydew3")
(set-face-attribute 'fringe nil :background "honeydew3")
(set-face-attribute 'mode-line nil :background "limegreen")
(set-face-attribute 'mode-line-inactive nil :background "lightgreen")
(set-face-attribute 'tab-bar nil
                    :background "honeydew3"
                    :foreground "yellow"
                    :height 1.5
                    :underline t
                    :bold t)


;; abbrev and skeletons config

(setq skeleton-end-newline nil)
(setq abbrev-suggest t)
(set-default 'abbrev-mode t)
(setq save-abbrevs nil)

(when (boundp 'global-abbrev-table)
  (clear-abbrev-table global-abbrev-table))

(define-abbrev-table 'global-abbrev-table
  '(
    ("dal" "$")
    ;; ("eq" "=")
    ("eeq" "==")
    ("eeeq" "===")
    ("sco" "_")
    ))


;; jacob-insert-config

(defun jacob-insert-block-helper (template)
  "Call `jacob-insert-helper' with a block appended to TEMPLATE."
  (jacob-insert-helper (concat template " {\n●\n}")))

(defun jacob-insert-assignment-helper (template)
  "Call `jacob-insert-helper' with \" ■ = \" appended to TEMPLATE."
  (jacob-insert-helper (concat template " ■ = ")))

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
  (jacob-insert-block-helper "for (■;●;●)"))

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
  (jacob-insert-helper "case ■: {\n●\nbreak;\n}"))

(define-jacob-insert jacob-insert-java-main
  (jacob-insert-block-helper "public static void main(String[] args)"))

(define-jacob-insert jacob-insert-java-print
  (jacob-insert-helper "System.out.println(■);"))

(define-jacob-insert jacob-insert-java-var
  (jacob-insert-assignment-helper "var"))

(define-jacob-insert jacob-insert-elisp-goto-char
  (jacob-insert-helper "(goto-char ■)"))

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
  (jacob-insert-helper "(■) => "))

(define-jacob-insert jacob-insert-js-for-each
  (jacob-insert-assignment-helper "forEach(■)"))

(define-jacob-insert jacob-insert-go-println
  (jacob-insert-helper "fmt.Println(■)"))

(define-jacob-insert jacob-insert-go-printf
  (jacob-insert-helper "fmt.Printf(■)"))

(define-jacob-insert jacob-insert-csharp-print
  (jacob-insert-helper "Console.WriteLine(■);"))

(define-jacob-insert jacob-insert-csharp-property
  (jacob-insert-helper "■ { get; set; }"))


;; icomplete config

;; (icomplete-mode 1)
;; (icomplete-vertical-mode 1)
;; (define-key icomplete-minibuffer-map (kbd "RET") 'icomplete-force-complete-and-exit)
;; (define-key icomplete-minibuffer-map (kbd "<tab>") 'icomplete-force-complete)


;; js-mode config

(put 'js-indent-level 'safe-local-variable #'numberp)

(defun jacob-js-config-hook-function ()
  "Configure `js-mode' when hook run."

  (when (boundp 'js-mode-abbrev-table)
    (clear-abbrev-table js-mode-abbrev-table))

  (define-abbrev-table 'js-mode-abbrev-table
    '(
      ("cl" "" jacob-insert-js-print)
      ("if" "" jacob-insert-c-if)
      ("for" "" jacob-insert-c-for)
      ("while" "" jacob-insert-c-while)
      ("switch" "" jacob-insert-c-switch)
      ("case" "" jacob-insert-c-case)
      ("fun" "" jacob-insert-js-function)
      ("con" "" jacob-insert-js-const)
      ("let" "" jacob-insert-js-let)
      ("eq" "===")
      ("neq" "!==")
      ("ret" "return")
      ("fore" "" jacob-insert-js-for-each)
      ("jwe" "console.log(\"jacobwozere\");" t)
      )))

(add-hook 'js-mode-hook 'jacob-js-config-hook-function)


;; cc-mode config

(setq-default c-basic-offset 4)

(when (boundp 'java-mode-abbrev-table)
  (clear-abbrev-table java-mode-abbrev-table))

(define-abbrev-table 'java-mode-abbrev-table
  '(
    ("sout" "" jacob-insert-java-print)
    ("psvm" "" jacob-insert-java-main)
    ("if" "" jacob-insert-c-if)
    ("for" "" jacob-insert-c-for)
    ("fore" "" jacob-insert-java-for-each)
    ("while" "" jacob-insert-c-while)
    ("string" "String")
    ("double" "Double")
    ("object" "Object")
    ("pri" "private")
    ("pub" "public")
    ("sta" "static")
    ("fin" "final")
    ("meth" "" jacob-insert-java-method)
    ("class" "" jacob-insert-java-class)
    ("cons" "" jacob-insert-java-constructor)
    ("var" "" jacob-insert-java-var)
    ("switch" "" jacob-insert-switch)
    ("case" "" jacob-insert-case)
    ("lt" "<")
    ("gt" ">")
    ("lte" "<=")
    ("gte" ">=")
    ("eq" "=")
    ("eeq" "==")
    ("neq" "!=")
    ("or" "||")
    ("and" "&&")
    ("ret" "return")
    ("char" "Character")
    ("inst" "instanceof")
    ))


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


;; emacs-lisp-mode config

(defun jacob-elisp-config-hook-function ()
  "Configure `emacs-lisp-mode' when hook run."
  (flymake-mode 1)
  (eldoc-mode 1)

  (define-abbrev-table 'emacs-lisp-mode-abbrev-table
    '(
      ("def" "" jacob-insert-elisp-defun)
      ("let" "" jacob-insert-lisp-let)
      ("int" "(interactive)" t)
      ("cond" "" jacob-insert-lisp-cond)
      ("gc" "" jacob-insert-elisp-goto-char)
      ("pmi" "(point-min)" t)
      ("pma" "(point-max)" t))))

(add-hook 'emacs-lisp-mode-hook 'jacob-elisp-config-hook-function)


;; font config

(defvar jacob-font-name
  (cond ((string-equal system-type "windows-nt")
         (when (member "Consolas" (font-family-list))
           "Consolas-"))
        ((string-equal system-type "darwin")
         (when (member "Menlo" (font-family-list))
           "Menlo-"))
        ((string-equal system-type "gnu/linux")
         (when (member "DejaVu Sans Mono" (font-family-list))
           "DejaVu Sans Mono-"))))

(defun jacob-set-font-size (size)
  "Set font to SIZE."
  (if (>= size 0)
      (let ((string-size (number-to-string size)))
        (set-frame-font (concat jacob-font-name string-size) nil t)
        (setq jacob-font-size size))
    nil))

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
(set-fontset-font
 t
 '(#x1f300 . #x1fad0)
 (cond
  ((member "Apple Color Emoji" (font-family-list)) "Apple Color Emoji")
  ((member "Noto Color Emoji" (font-family-list)) "Noto Color Emoji")
  ((member "Noto Emoji" (font-family-list)) "Noto Emoji")
  ((member "Segoe UI Emoji" (font-family-list)) "Segoe UI Emoji")
  ((member "Symbola" (font-family-list)) "Symbola")))


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
                   jacob-move-to-window-line-top
                   jacob-move-to-window-line-centre
                   jacob-move-to-window-line-bottom
                   jacob-recenter-top
                   jacob-recenter-centre
                   jacob-recenter-bottom
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

(if (and (boundp 'jacob-raspberry-pi-ip-address) (boundp 'jacob-raspberry-pi-connection-string))
    (setq diary-file (concat jacob-raspberry-pi-connection-string "/home/pi/org/jacobsDiary.diary")))

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
  (if (or (string= major-mode "emacs-lisp-mode")
          (string= major-mode "racket-mode")
          (string= major-mode "csharp-tree-sitter-mode")
          (string= major-mode "sml-mode")
          (string= major-mode "java-mode"))
      (indent-region (point-min) (point-max))))

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
                         web-mode
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
                         ;; misc
                         restart-emacs
                         docker-tramp)
  "List of packages to install.")

(defun jacob-wrangle-packages ()
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

(require 'jacob-package-configuration)



;; personal functions

(require 'jacob-personal-functions)



;; key bindings

(require 'jacob-keybinds)



(provide 'init)
;;; init.el ends here
