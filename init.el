;;; init.el --- Jacob's main init file. -*-lexical-binding: t-*-
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


;; misc

(add-to-list 'load-path "~/.emacs.d/local-packages/")

(setq kill-buffer-query-functions (delq 'process-kill-buffer-query-function
                                        kill-buffer-query-functions))
(setq read-process-output-max (* 1024 1024))
(setq ring-bell-function 'ignore)
(setq auto-window-vscroll nil)
(setq scroll-conservatively 100)
(setq create-lockfiles nil)
(setq history-length 1000)
(setq history-delete-duplicates t)
(setq-default truncate-lines nil)
(defalias 'yes-or-no-p 'y-or-n-p)


;; unicode

(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)


;; prettify-symbols-mode

(defun jacob-racket-setup-prettify-symbols ()
  "Make some word or string show as pretty Unicode symbols."
  (setq prettify-symbols-alist
        '(
          ("lambda" . 955) ; λ
          ("->" . 8594)    ; →
          ("=>" . 8658)    ; ⇒
          ("map" . 8614)   ; ↦
          ("<=" . 8804)
          (">=" . 8805)
          ))
  (prettify-symbols-mode 1))

(add-hook 'racket-mode-hook 'jacob-racket-setup-prettify-symbols)


;; misc 2: electric boogaloo

(setq confirm-kill-processes nil)
(setq backup-by-copying t)
(setq make-backup-files nil)
(setq auto-save-default nil)

(add-hook 'after-init-hook (lambda () (recentf-mode 1)))

(setq dabbrev-case-fold-search nil)
(setq dabbrev-case-replace nil)

(setq custom-file (make-temp-file "emacs-custom-"))

(setq disabled-command-function nil)

(setq line-move-visual t)

(defun jacob-prog-mode-hook-function ()
  "Hook function to run when in programming mode."
  (global-subword-mode 1)

  (setq show-paren-style 'parenthesis)
  (setq show-paren-when-point-inside-paren t)
  (setq show-paren-when-point-in-periphery nil)
  (show-paren-mode 1)

  (setq electric-pair-preserve-balance t)
  (setq electric-pair-delete-adjacent-pairs t)
  (setq electric-pair-open-newline-between-pairs t)
  (electric-pair-mode 1)

  (remove-hook 'prog-mode-hook 'jacob-prog-mode-hook-function))

(add-hook 'prog-mode-hook 'jacob-prog-mode-hook-function)

(setq ibuffer-expert t)

(defun jacob-find-file-hook-function ()
  "Hook function to run after file openend."
  (setq savehist-file "~/.emacs.d/savehist")
  (setq savehist-save-minibuffer-history t)
  (savehist-mode 1)

  (setq save-place-file "~/.emacs.d/saveplace")
  (setq save-place-forget-unreadable-files t)
  (save-place-mode 1)

  (delete-selection-mode 1)
  (auto-save-visited-mode 1)

  (remove-hook 'find-file-hook 'jacob-find-file-hook-function))

(add-hook 'find-file-hook 'jacob-find-file-hook-function)

(setq use-file-dialog nil)
(setq use-dialog-box t)
(setq inhibit-startup-message nil)


;; mode line

(setq-default mode-line-format (list "%*" ; saved, readonly
                                     "%m: " ; major mode
                                     "%b " ; buffer name
                                     "(%l,%c)" ; position of point
                                     ))


;; abbrev-mode

(setq skeleton-end-newline nil)

(clear-abbrev-table global-abbrev-table)

(define-abbrev-table 'global-abbrev-table
  '(

    ))

(when (boundp 'python-mode-abbrev-table)
  (clear-abbrev-table python-mode-abbrev-table))

(define-abbrev-table 'python-mode-abbrev-table
  '(
    ("key" "\"\": Key(\"\"),")
    ))

(set-default 'abbrev-mode t)

(setq save-abbrevs nil)


;; javascript-mode

(define-skeleton jacob-javascript-skeleton-console-log
  "insert console.log" nil
  > "console.log(" - ");")

(define-skeleton jacob-javascript-skeleton-if
  "insert if statement" nil
  > "if (" - ") {" \n
  \n
  -2 "}")

(define-skeleton jacob-javascript-skeleton-arrow-function
  "insert arrow function" nil
  > "const " - " = () => {" \n
  \n
  -2 "}")

(when (boundp 'js-mode-abbrev-table)
  (clear-abbrev-table js-mode-abbrev-table))

(define-abbrev-table 'js-mode-abbrev-table
  '(
    ("cl" "" jacob-javascript-skeleton-console-log)
    ("if" "" jacob-javascript-skeleton-if)
    ("arr" "" jacob-javascript-skeleton-arrow-function)
    ))


;; cc-mode config

(setq-default c-basic-offset 4)

(define-skeleton jacob-java-sout
  "insert System.out.println()" nil
  > "System.out.println(" - ")")

(define-skeleton jacob-java-main
  "insert main method." nil
  > "public static void main(String[] args) {" \n
  - \n
  -4 "}")

(define-skeleton jacob-java-if
  "insert if statement." nil
  > "if (" - ") {" \n
  \n
  -4 "}")

(define-skeleton jacob-java-for
  "insert for statement." nil
  > "for (" - ") {" \n
  \n
  -4 "}")

(when (boundp 'java-mode-abbrev-table)
  (clear-abbrev-table java-mode-abbrev-table))

(define-abbrev-table 'java-mode-abbrev-table
  '(
    ("sout" "" jacob-java-sout)
    ("psvm" "" jacob-java-main)
    ("if" "" jacob-java-if)
    ("for" "" jacob-java-for)
    ))


;; dired-mode config

(with-eval-after-load 'dired
  (setq dired-recursive-copies 'always)
  (setq dired-recursive-deletes 'top)
  (setq dired-dwim-target t)

  (defun jacob-dired-mode-setup ()
    "Hook function for dired."
    (require 'dired-x)
    (setq dired-guess-shell-alist-user '(("\\.mkv\\'" "mpv")))
    (dired-hide-details-mode 1))

  (add-hook 'dired-mode-hook 'jacob-dired-mode-setup))


;; emacs-lisp-mode config

(defun jacob-elisp-config-hook-function ()
  "Configure `emacs-lisp-mode' when hook run."
  (progn
    (when (not buffer-display-table)
      (setq buffer-display-table (make-display-table)))
    (aset buffer-display-table ?\^L
          (vconcat (make-list 70 (make-glyph-code ?─ 'font-lock-comment-face))))
    (redraw-frame))
  (flymake-mode 1)
  (eldoc-mode 1)
  (define-skeleton jacob-emacs-lisp-skeleton-let
    "insert let" nil
    > "(let ((" - "))" \n
    ")")

  (define-skeleton jacob-emacs-lisp-skeleton-defun
    "insert defun" nil
    > "(defun " - " ()" \n
    -2 "\"\"" \n
    ")")

  (when (boundp 'emacs-lisp-mode-abbrev-table)
    (clear-abbrev-table emacs-lisp-mode-abbrev-table))

  (define-abbrev-table 'emacs-lisp-mode-abbrev-table
    '(
      ("let" "" jacob-emacs-lisp-skeleton-let)
      ("defun" "" jacob-emacs-lisp-skeleton-defun)
      )))

(add-hook 'emacs-lisp-mode-hook 'jacob-elisp-config-hook-function)


;; font config

(cond
 ((string-equal system-type "windows-nt")
  (when (member "Consolas" (font-family-list))
    (set-frame-font "Consolas-12" nil t)))
 ((string-equal system-type "darwin")
  (when (member "Menlo" (font-family-list))
    (set-frame-font "Menlo-12" nil t)))
 ((string-equal system-type "gnu/linux")
  (when (member "DejaVu Sans Mono" (font-family-list))
    (set-frame-font "DejaVu Sans Mono-12" nil t))))


;; org config

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
   '((octave . t))))


;; pulse config

(defun jacob-pulse-line (&rest _)
  "Pulse the current line."
  (pulse-momentary-highlight-region (+ (line-beginning-position)
                                       (current-indentation))
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

(load "server")
(server-start)


;; time emacs startup

(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs ready in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))


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
          (string= major-mode "sml-mode"))
      (indent-region (point-min) (point-max))))

(add-hook 'before-save-hook 'jacob-indent-with-major-mode)


;; microsoft windows config

(when (eq system-type 'windows-nt)
  (setq w32-pass-rwindow-to-system nil)
  (setq w32-rwindow-modifier 'super)
  (setq w32-apps-modifier 'hyper)

  (setq ls-lisp-use-insert-directory-program nil)
  (setq ls-lisp-dirs-first t)

  (add-hook 'after-init-hook (lambda ()
                               ;; maximize window
                               (w32-send-sys-command 61488))))


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



;; package installation

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
                         ("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")
                         ))

(setq package-selected-packages '(
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
                                  selectrum
                                  consult
                                  orderless
                                  prescient
                                  selectrum-prescient
                                  marginalia
                                  edit-server
                                  goto-last-change
                                  eglot
                                  expand-region
                                  projectile
                                  restart-emacs
                                  which-key
                                  xah-fly-keys
                                  xah-find
                                  modus-themes
                                  magit
                                  tsc
                                  tree-sitter-langs
                                  tree-sitter-indent
                                  tree-sitter
                                  ))

(unless (string= (package-install-selected-packages) "All your packages are already installed")
  (package-refresh-contents)
  (package-install-selected-packages))
(package-autoremove)

(package-initialize)



;; package configuration


;; themes

(load-theme 'modus-operandi t)


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



(jacob-try-require 'edit-server
  (edit-server-start))


;; csharp-mode

(jacob-is-installed 'csharp-mode
  (add-to-list 'auto-mode-alist '("\\.cs\\'" . csharp-tree-sitter-mode))
  (with-eval-after-load 'csharp-tree-sitter

    (define-skeleton jacob-csharp-skeleton-console-writeline
      "insert console.writeline" nil
      > "Console.WriteLine(" - ")")

    (define-skeleton jacob-csharp-skeleton-if
      "insert if statement" nil
      > "if (" - ")" \n
      -4 "{"\n
      \n
      -4 "}")
    
    (when (boundp 'csharp-tree-sitter-mode-abbrev-table)
      (clear-abbrev-table csharp-tree-sitter-mode-abbrev-table))
    
    (define-abbrev-table 'csharp-tree-sitter-mode-abbrev-table
      '(
        ("cwl" "" jacob-csharp-skeleton-console-writeline)
        ("if" "" jacob-csharp-skeleton-if)
        ("pu" "public")
        ("pr" "private")
        ("as" "async")
        ("st" "static")
        ))))



(jacob-is-installed 'eglot
  (add-hook 'java-mode-hook 'eglot-ensure)
  (add-hook 'csharp-tree-sitter-mode-hook 'eglot-ensure)
  (add-hook 'typescript-mode-hook 'eglot-ensure)
  (with-eval-after-load 'eglot
    (setcdr (assq 'java-mode eglot-server-programs) #'jacob-eglot-eclipse-jdt-contact)
    (add-to-list 'eglot-server-programs '((js-mode typescript-mode) . ("typescript-language-server" "--stdio")))

    (add-to-list 'eglot-server-programs `(csharp-tree-sitter-mode . ("d:/programming/omnisharp-roslyn-1.37.15/artifacts/publish/OmniSharp.Stdio.Driver/win7-x64/OmniSharp.exe" "-lsp")))
    (add-to-list 'eglot-server-programs `(web-mode . ("typescript-language-server" "--stdio")))

    (defun jacob-eglot-eclipse-jdt-contact
        (interactive)
      "Contact with the jdt server input INTERACTIVE."
      (let ((cp (getenv "CLASSPATH"))
            (jdt-home jacob-eclipse-jdt-file-path))
        (setenv "CLASSPATH" (concat cp ":" jdt-home))
        (unwind-protect (eglot--eclipse-jdt-contact nil)
          (setenv "CLASSPATH" cp))))

    (defun eglot--make-diag (buffer
                             beg
                             end
                             type
                             text
                             &optional data
                             overlay-properties)
      "Make a Flymake diagnostic for BUFFER's region from BEG to END.
TYPE is a diagnostic symbol and TEXT is string describing the
problem detected in this region.  DATA is any object that the
caller wishes to attach to the created diagnostic for later
retrieval.
 
OVERLAY-PROPERTIES is an alist of properties attached to the
created diagnostic, overriding the default properties and any
properties of `flymake-overlay-control' of the diagnostic's
type.

tweaked to implement a hack by me. can be deleted once i have
made typescript flymake."
      (if (not (string= text "typescript: Experimental support for decorators is a feature that is subject to change in a future release. Set the 'experimentalDecorators' option in your 'tsconfig' or 'jsconfig' to remove this warning."))
          (flymake--diag-make :buffer buffer :beg beg :end end
                              :type type :text text :data data
                              :overlay-properties overlay-properties)
        (flymake--diag-make :buffer buffer :beg 0 :end 0
                            :type type :text text :data data
                            :overlay-properties overlay-properties)))))


;; selectrum config

(jacob-try-require 'selectrum
  (jacob-try-require 'orderless
    (setq completion-styles '(orderless)))

  (jacob-try-require 'prescient
    (jacob-try-require 'selectrum-prescient
      (selectrum-prescient-mode 1))
    (prescient-persist-mode 1))

  (jacob-try-require 'marginalia
    (marginalia-mode 1))

  (setq selectrum-display-action nil)
  (setq selectrum-max-window-height 25)
  (setq enable-recursive-minibuffers t)
  (selectrum-mode 1))

(jacob-is-installed 'consult
  (with-eval-after-load 'consult
    (setq completion-in-region-function 'consult-completion-in-region)
    (setq consult-preview-key 'any)
    (dolist (command '(consult-bookmark consult-recent-file consult-buffer))
      (setf (alist-get command consult-config) `(:preview-key ,nil)))))



(jacob-is-installed 'expand-region
  (with-eval-after-load 'expand-region
    (setq expand-region-contract-fast-key "9")))



(jacob-is-installed 'projectile
  (projectile-mode 1)

  (with-eval-after-load 'projectile
    (setq projectile-completion-system 'default)
    (setq projectile-indexing-method 'native)))



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
      " => ")

    (when (boundp 'sml-mode-abbrev-table)
      (clear-abbrev-table sml-mode-abbrev-table))
    
    (define-abbrev-table 'sml-mode-abbrev-table
      '(
        ("val" "" jacob-sml-skeleton-val)
        ("if" "" jacob-sml-skeleton-if)
        ("let" "" jacob-sml-skeleton-let)
        ("fun" "" jacob-sml-skeleton-function)
        ("fn" "" jacob-sml-skeleton-anonymous-function)
        ("case" "" jacob-sml-skeleton-case)
        ))))



(defun jacob-prettier-format-buffer ()
  "Use prettier to format current buffer."
  (interactive)
  (when (or (eq major-mode 'web-mode)
            (eq major-mode 'typescript-mode)
            (eq major-mode 'javascript-mode))
    (let* ((temporary-file-extension (file-name-extension (buffer-file-name) t))
           (temporary-file (concat "jacob-prettier-temporary-file" temporary-file-extension)))
      (append-to-file (point-min) (point-max) temporary-file)
      (call-process "npx" nil nil t "prettier" temporary-file "--write")
      (erase-buffer)
      (insert-file-contents temporary-file)
      (delete-file temporary-file))))

(jacob-is-installed 'typescript-mode
  (with-eval-after-load 'typescript-mode

    (setq typescript-indent-level 4)

    (define-skeleton jacob-typescript-skeleton-console-log
      "insert console.log" nil
      > "console.log(" - ");")

    (define-skeleton jacob-typescript-skeleton-if
      "insert if statement" nil
      > "if (" - ") {" \n
      \n
      -2 "}")

    (define-skeleton jacob-typescript-skeleton-arrow-function
      "insert arrow function" nil
      > "const " - " = () => {" \n
      \n
      -2 "}")
    
    (when (boundp 'typescript-mode-abbrev-table)
      (clear-abbrev-table typescript-mode-abbrev-table))
    
    (define-abbrev-table 'typescript-mode-abbrev-table
      '(
        ("cl" "" jacob-typescript-skeleton-console-log)
        ("if" "" jacob-typescript-skeleton-if)
        ("arr" "" jacob-typescript-skeleton-arrow-function)
        ))))



(jacob-is-installed 'web-mode
  (defun jacob-web-mode-config ()
    (if (string= (file-name-extension (buffer-name)) "tsx")
        (eglot-ensure))
    (setq-local electric-pair-pairs '((?\" . ?\") (?\< . ?\>))))

  (add-hook 'web-mode-hook 'jacob-web-mode-config)

  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.cshtml\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.css\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
  
  (with-eval-after-load 'web-mode
    (add-to-list 'web-mode-engines-alist '("razor" . "\\.cshtml\\'"))
    (setq web-mode-markup-indent-offset 2)
    (setq web-mode-css-indent-offset 2)
    (setq web-mode-code-indent-offset 2)))



(jacob-is-installed 'which-key
  (which-key-mode 1))



(setq xah-fly-use-control-key nil)
(setq xah-fly-use-meta-key t)

(jacob-try-require 'xah-fly-keys
  (xah-fly-keys-set-layout "qwerty")
  (xah-fly-keys 1)

  (defun xah-jacob-kill-word (repetitions)
    (interactive "p")
    (when (use-region-p)
      (delete-region (region-beginning) (region-end)))
    (kill-word repetitions))

  (defun xah-jacob-backward-kill-word (repetitions)
    (interactive "p")
    (when (use-region-p)
      (delete-region (region-beginning) (region-end)))
    (backward-kill-word repetitions))

  (defun xah-jacob-beginning-of-line-or-block (repetitions)
    (interactive "p")
    (dotimes (i repetitions)
      (xah-beginning-of-line-or-block)))

  (defun xah-jacob-end-of-line-or-block (repetitions)
    (interactive "p")
    (dotimes (i repetitions)
      (xah-end-of-line-or-block)))

  (add-hook 'dired-mode-hook 'xah-fly-insert-mode-activate))



;; personal functions

(defun jacob-words-to-symbol (begin end)
  ""
  (interactive "r")
  (if (region-active-p)
      (let* ((simple-template (lambda (char begin end)
                                (save-restriction
                                  (narrow-to-region begin end)
                                  (goto-char (point-min))
                                  (while (search-forward " " nil t)
                                    (replace-match char)))))
             (hyphenate (lambda (begin end)
                          (funcall simple-template "-" begin end)))
             (underscore (lambda (begin end)
                           (funcall simple-template "_" begin end))))
        (cond
         ((eq major-mode 'emacs-lisp-mode)
          (funcall hyphenate begin end))
         ((eq major-mode 'sml-mode)
          (funcall underscore begin end))
         (t
          (funcall underscore begin end))))
    (message "No selection.")))

(defun jacob-create-camel-case-variable-name ()
  "Ask for input, apply camel case to input and insert at point."
  (interactive)
  (let* ((input (read-string "Enter words to be camel cased:"))
         (input-list (split-string input " ")))
    (insert (concat (car input-list)
                    (mapconcat 'capitalize
                               (cdr input-list)
                               "")))))

(defun jacob-create-pascal-case-variable-name ()
  "Ask for input, apply pascal case to input and insert at point."
  (interactive)
  (let* ((input (read-string "Enter words to be pascal cased:"))
         (input-list (split-string input " ")))
    (insert (mapconcat 'capitalize
                       input-list
                       ""))))

(defun jacob-create-hyphenated-variable-name ()
  "Ask for input, hyphenate the input and insert at point."
  (interactive)
  (let* ((input (read-string "Enter words to be hyphenated:"))
         (output (with-temp-buffer
                   (insert input)
                   (goto-char (point-min))
                   (while (search-forward " " nil t)
                     (replace-match "-"))
                   (buffer-string))))
    (insert output)))

(defun jacob-create-underscored-variable-name ()
  "Ask for input, underscore the input and insert at point."
  (interactive)
  (let* ((input (read-string "Enter words to be underscored:"))
         (output (with-temp-buffer
                   (insert input)
                   (goto-char (point-min))
                   (while (search-forward " " nil t)
                     (replace-match "_"))
                   (buffer-string))))
    (insert output)))

(defun jacob-count-words-region ()
  "If mark active count words in region, otherwise count words in whole buffer."
  (interactive)
  (if mark-active
      (call-interactively 'count-words-region)
    (let ((current-prefix-arg t))
      (call-interactively 'count-words-region))))

(define-key global-map (kbd "M-=") 'jacob-count-words-region)

(defun jacob-original-find-file ()
  "Uses the original file-file mechanism.
  Useful for dealing with files on other servers.
  (at least on Microsoft Windows)"
  (interactive)
  (let ((completing-read-function 'completing-read-default))
    (call-interactively 'find-file)))

(defun jacob-eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))

(defun jacob-quit-popup-window ()
  (interactive)
  (let ((loop-list (window-list))
        (window-not-found t))
    (while (and loop-list window-not-found)
      (let* ((window (car loop-list))
             (mode (jacob-buffer-mode (window-buffer window))))
        (if (or (eq mode 'help-mode)
                (eq mode 'compilation-mode)
                (eq mode 'special-mode))
            (progn
              (quit-window :window window)
              (setq window-found nil))))
      (setq loop-list (cdr loop-list)))))

(defun jacob-buffer-mode (buffer-or-string)
  "Return the major mode associated with BUFFER-OR-STRING."
  (with-current-buffer buffer-or-string
    major-mode))

(defun jacob-config-visit ()
  "Open the init file."
  (interactive)
  (find-file "~/.emacs.d/init.el"))

(defun jacob-config-reload ()
  "Evaluate the init file."
  (interactive)
  (load-file (expand-file-name "~/.emacs.d/init.el")))

(defun jacob-org-src-block ()
  "Replacement for `C-c '` in both `org-mode' and when editing code blocks within `org-mode'."
  (interactive)
  (if (bound-and-true-p org-src-mode)
      (org-edit-src-exit)
    (if (equal major-mode 'org-mode)
        (org-edit-special))))

(defun jacob-recompile-packages ()
  "Recompile all packages."
  (interactive)
  (byte-recompile-directory package-user-dir nil 'force))

(defun jacob-split-window-below-select-new ()
  "Splits current window vertically, then switch to new window."
  (interactive)
  (split-window-below)
  (other-window 1))

(defun jacob-split-window-right-select-new ()
  "Splits current window horizontally, then switch to new window."
  (interactive)
  (split-window-right)
  (other-window 1))

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

(defun jacob-xah-insert-bracket-pair (@left-bracket @right-bracket)
  "Heavily simplified version of Xah's excellent function. 
My usecases differ to his, so I have removed a vast amount of the functionality. 
Now, this function will insert a pair, or wrap the region if it is active.

Original Version can be found here:
URL `http://ergoemacs.org/emacs/elisp_insert_brackets_by_pair.html'"

  (if (use-region-p)
      (let (($p1 (region-beginning))
            ($p2 (region-end)))
        (goto-char $p2)
        (insert @right-bracket)
        (goto-char $p1)
        (insert @left-bracket)
        (goto-char (+ $p2 2)))
    (let ($p1 $p2)
      (setq $p1 (point) $p2 (point))
      (insert @left-bracket @right-bracket)
      (search-backward @right-bracket))))

(defun jacob-back-to-indentation-or-beginning-of-line ()
  "Do back-to-indentation unless at end of indentation
in which case do move-beginning-of-line."
  (interactive)
  (if (and (not (equal (point) (line-beginning-position)))
           (eq last-command this-command))
      (move-beginning-of-line nil)
    (back-to-indentation)))

(defun jacob-xah-insert-paren ()
  (interactive)
  (jacob-xah-insert-bracket-pair "(" ")"))

(defun jacob-xah-insert-square-bracket ()
  (interactive)
  (jacob-xah-insert-bracket-pair "[" "]"))

(defun jacob-xah-insert-brace ()
  (interactive)
  (jacob-xah-insert-bracket-pair "{" "}"))

(defun jacob-xah-insert-ascii-double-quote ()
  (interactive)
  (jacob-xah-insert-bracket-pair "\"" "\""))

(defun jacob-xah-insert-ascii-single-quote ()
  (interactive)
  (jacob-xah-insert-bracket-pair "'" "'"))

(defun jacob-xah-insert-angled-bracket ()
  (interactive)
  (jacob-xah-insert-bracket-pair "<" ">"))

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

(fset 'jacob-enter-kmacro
      [return])

(fset 'jacob-backspace-kmacro
      [?f backspace home])

(defun jacob-matlab-matrix-to-latex (matrix-start matrix-end)
  (interactive (if (use-region-p)
                   (list (region-beginning) (region-end))
                 (let (region-start
                       region-end)
                   (search-backward "[")
                   (setq region-start (point))
                   (search-forward "]")
                   (setq region-end (point))
                   (list region-start region-end))))
  (save-excursion
    (save-restriction
      (narrow-to-region matrix-start matrix-end)

      (progn
        (goto-char (point-min))
        (while (search-forward-regexp "[[:space:]]+" nil t)
          (replace-match " ")))
      
      (dolist (pair (list (quote ("[ " "["))
                          (quote ("[" "\\\\jbmat{"))
                          (quote (" ]" "]"))
                          (quote ("]" "}"))
                          (quote ("; " ";"))
                          (quote (" " " & "))
                          (quote (";" " \\\\\\\\ "))))
        (progn
          (goto-char (point-min))
          (while (search-forward (car pair) nil t)
            (replace-match (car (last pair)))))))))

(defun jacob-system-shutdown ()
  "Prompts for yes/no input.

If user inputs yes, system is shutdown. Otherwise, nothing happens."
  (interactive)
  (if (yes-or-no-p "Shutdown system?")
      (shell-command "pwsh -Command Stop-Computer")))



;; voice commands

(defun jacob-recenter-top ()
  (interactive)
  (recenter 5))

(defun jacob-recenter-centre ()
  (interactive)
  (recenter))

(defun jacob-recenter-bottom ()
  (interactive)
  (recenter -5))

(defun jacob-move-to-window-line-top ()
  (interactive)
  (move-to-window-line 5))

(defun jacob-move-to-window-line-centre ()
  (interactive)
  (move-to-window-line nil))

(defun jacob-move-to-window-line-bottom ()
  (interactive)
  (move-to-window-line -5))

(defun jacob-voice-mark-command ()
  (interactive)
  (if (region-active-p)
      (er/expand-region 1)
    (set-mark (point))))

(defun jacob-switch-to-previous-buffer ()
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))



;; key bindings


;; voice command keybindings

(global-unset-key (kbd "C-z"))

(let ((map global-map))
  (define-key map (kbd "C-z r") 'xah-jacob-kill-word)
  (define-key map (kbd "C-z e") 'xah-jacob-backward-kill-word)
  (define-key map (kbd "C-z h") 'xah-jacob-beginning-of-line-or-block)
  (define-key map (kbd "C-z ;") 'xah-jacob-end-of-line-or-block)
  (jacob-is-installed 'xah-fly-keys
    (define-key map (kbd "C-z c") 'xah-copy-line-or-region)
    (define-key map (kbd "C-z x") 'xah-cut-line-or-region)
    (define-key map (kbd "C-z .") 'xah-forward-right-bracket)
    (define-key map (kbd "C-z m") 'xah-backward-left-bracket)
    (define-key map (kbd "C-z /") 'xah-goto-matching-bracket)
    (define-key map (kbd "C-z d") 'xah-delete-backward-char-or-bracket-text)
    (define-key map (kbd "C-z 0") 'xah-pop-local-mark-ring)
    (define-key map (kbd "C-z v") 'xah-paste-or-paste-previous)
    (define-key map (kbd "C-z w") 'xah-shrink-whitespaces))
  (define-key map (kbd "C-z p p") 'jacob-xah-insert-paren)
  (define-key map (kbd "C-z p b") 'jacob-xah-insert-square-bracket)
  (define-key map (kbd "C-z p c") 'jacob-xah-insert-brace)
  (define-key map (kbd "C-z p a") 'jacob-xah-insert-angled-bracket)
  (define-key map (kbd "C-z p q") 'jacob-xah-insert-ascii-double-quote)
  (define-key map (kbd "C-z p s") 'jacob-xah-insert-ascii-single-quote)
  (define-key map (kbd "M-=") 'jacob-count-words-region)
  (define-key map (kbd "C-z C-l t") 'jacob-recenter-top)
  (define-key map (kbd "C-z C-l c") 'jacob-recenter-centre)
  (define-key map (kbd "C-z C-l b") 'jacob-recenter-bottom)
  (define-key map (kbd "C-z M-r t") 'jacob-move-to-window-line-top)
  (define-key map (kbd "C-z M-r c") 'jacob-move-to-window-line-centre)
  (define-key map (kbd "C-z M-r b") 'jacob-move-to-window-line-bottom)
  (jacob-is-installed 'consult
    (define-key map (kbd "C-z SPC v") 'consult-yank-from-kill-ring)
    (define-key map (kbd "C-z SPC i j") 'consult-recent-file)
    (define-key map (kbd "C-z SPC e c f") 'consult-buffer)
    (define-key map (kbd "C-z SPC e c n") 'consult-line))
  (jacob-is-installed 'projectile
    (define-key map (kbd "C-z SPC e p") 'projectile-command-map))
  (define-key map (kbd "C-z t") 'jacob-voice-mark-command)
  (define-key map (kbd "C-x 2") 'jacob-split-window-below-select-new)
  (define-key map (kbd "C-x 3") 'jacob-split-window-right-select-new)
  (define-key map (kbd "C-z f") 'jacob-switch-to-previous-buffer)
  (define-key map (kbd "C-z F") 'ibuffer)
  (jacob-is-installed 'goto-last-change
    (define-key map (kbd "C-z j") 'goto-last-change)
    (define-key map (kbd "C-z l") 'goto-last-change-reverse)))


;; xah-fly-keys keybindings

(jacob-is-installed 'xah-fly-keys
  (define-prefix-command 'jacob-config-keymap)
  (define-prefix-command 'jacob-eglot-keymap)

  (jacob-is-installed 'eglot
    (let ((map jacob-eglot-keymap))
      (define-key map (kbd "a") 'eglot-code-actions)
      (define-key map (kbd "r") 'eglot-rename)))

  (let ((map xah-fly-dot-keymap))
    ;; (define-key map (kbd "v") vc-prefix-map)
    (define-key map (kbd "c") jacob-config-keymap)
    (jacob-is-installed 'eglot
      (define-key map (kbd "e") jacob-eglot-keymap))
    (jacob-is-installed 'consult
      (define-key map (kbd "s") 'consult-line))
    ;; (define-key map (kbd "p") project-prefix-map)
    (jacob-is-installed 'projectile
      (define-key map (kbd "p") 'projectile-command-map))
    (jacob-is-installed 'magit
      (define-key map (kbd "v") 'magit-status))
    (jacob-is-installed 'modus-themes
      (define-key map (kbd "t") 'modus-themes-toggle)))

  (let ((map xah-fly-command-map))
    (define-key map (kbd "a") 'execute-extended-command)
    (define-key map (kbd "s") (kbd "RET"))
    (define-key map (kbd "DEL") nil)
    (define-key map (kbd "4") 'jacob-split-window-below-select-new)
    (define-key map (kbd "1") 'winner-undo)
    (define-key map (kbd "2") 'winner-redo)
    (jacob-is-installed 'expand-region
      (define-key map (kbd "8") 'er/expand-region)))

  (let ((map jacob-config-keymap))
    (define-key map (kbd "r") 'jacob-config-reload)
    (define-key map (kbd "e") 'jacob-config-visit)
    (define-key map (kbd "c") 'jacob-org-src-block)
    (define-key map (kbd "p") 'jacob-recompile-packages)
    (define-key map (kbd "t") 'jacob-display-time)
    (jacob-is-installed 'restart-emacs
      (define-key map (kbd "R") 'restart-emacs)))

  (let ((map xah-fly-e-keymap))
    (define-key map (kbd "k") 'jacob-xah-insert-paren)
    (define-key map (kbd "l") 'jacob-xah-insert-square-bracket)
    (define-key map (kbd "j") 'jacob-xah-insert-brace)
    (define-key map (kbd "u") 'jacob-xah-insert-ascii-double-quote)
    (define-key map (kbd "i") 'jacob-xah-insert-ascii-single-quote)
    (define-key map (kbd "m") 'xah-insert-hyphen)
    (define-key map (kbd ",") 'xah-insert-low-line)
    (define-key map (kbd ".") 'jacob-insert-equals)
    (define-key map (kbd "/") 'jacob-insert-plus)
    (define-key map (kbd "z") 'jacob-insert-apostrophe)
    (define-key map (kbd "x") 'jacob-insert-at)
    (define-key map (kbd "c") 'jacob-insert-hash)
    (define-key map (kbd "d") (kbd "DEL"))
    (define-key map (kbd "v") 'jacob-insert-tilde)
    (define-key map (kbd "e") 'jacob-insert-dollar-sign)
    (define-key map (kbd "r") 'jacob-insert-caret)
    (define-key map (kbd "o") 'jacob-insert-ampersand))

  (let ((map xah-fly-leader-key-map))
    (define-key map (kbd "4") 'jacob-split-window-right-select-new)
    (jacob-is-installed 'consult
      (define-key map (kbd "v") 'consult-yank)
      (define-key map (kbd "f") 'consult-buffer)))

  (let ((map xah-fly-w-keymap))
    (define-key map (kbd "n") 'jacob-eval-and-replace))

  (let ((map xah-fly-t-keymap))
    (define-key map (kbd "j") 'kill-this-buffer))

  (let ((map xah-fly-c-keymap))
    (define-key map (kbd "j") 'consult-recent-file)
    (define-key map (kbd "e") 'find-file))

  (let ((map xah-fly-r-keymap)) 
    (define-key map (kbd "c") 'kmacro-set-counter))

  ;; dired rebinding
  (let ((map dired-mode-map))
    (define-key map (kbd "p") 'dired-maybe-insert-subdir)
    (define-key map (kbd "n") 'dired-do-kill-lines)
    (define-key map (kbd "i") 'dired-previous-line)
    (define-key map (kbd "k") 'dired-next-line)
    (define-key map (kbd "a") 'execute-extended-command)
    (define-key map (kbd "s") 'dired-find-file)
    
    ))


;; keys for inserting symbols

(with-eval-after-load 'xah-fly-keys
  (define-prefix-command 'jacob-symbol-insertion-map)
  (define-prefix-command 'jacob-programming-name-insertion-map)

  (define-key xah-fly-insert-map (kbd "`") 'jacob-symbol-insertion-map)

  (let ((map jacob-symbol-insertion-map))
    (define-key map (kbd "`") (lambda () (interactive) (insert "`")))

    (define-key map (kbd "j") (kbd "{"))
    (define-key map (kbd "k") (kbd "("))
    (define-key map (kbd "l") (kbd "["))
    (define-key map (kbd "u") (kbd "\""))
    (define-key map (kbd "i") (kbd "'"))

    (define-key map (kbd "m") (kbd "-"))
    (define-key map (kbd ",") (kbd "_"))
    (define-key map (kbd ".") (kbd "="))
    (define-key map (kbd "/") (kbd "+"))

    (define-key map (kbd "v") (kbd "~"))
    (define-key map (kbd "c") (kbd "#"))
    (define-key map (kbd "x") (kbd "@"))

    (define-key map (kbd "z") 'jacob-programming-name-insertion-map))

  (let ((map jacob-programming-name-insertion-map))
    (define-key map (kbd "p") 'jacob-create-pascal-case-variable-name)
    (define-key map (kbd "c") 'jacob-create-camel-case-variable-name)
    (define-key map (kbd "h") 'jacob-create-hyphenated-variable-name)
    (define-key map (kbd "u") 'jacob-create-underscored-variable-name)))



(provide 'init)
;;; init.el ends here
