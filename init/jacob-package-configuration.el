;;; jacob-package-configuration.el --- my configuration for packages

;; racket-mode


;;; Commentary:
;; 

;;; Code:

(jacob-is-installed 'racket-mode
  (add-hook 'racket-mode-hook 'racket-xp-mode))


;; go-mode

(jacob-is-installed 'go-mode

  (define-abbrev-table 'go-mode-abbrev-table
    '(
      ("fpl" "" jacob-insert-go-println)
      ("fpf" "" jacob-insert-go-printf)
      )))


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
  (add-to-list 'auto-mode-alist '("\\.cs\\'" . csharp-tree-sitter-mode))
  (with-eval-after-load 'csharp-tree-sitter

    ;; possibly reintroduce if i ever do csharp again seriously
    ;; (define-skeleton jacob-csharp-class
    ;;   "insert class"
    ;;   > "public "
    ;;   (let ((case-fold-search nil)
    ;;         (file-name (file-name-base (buffer-file-name))))
    ;;     (concat (if (string-match "^I.+" file-name)
    ;;                 "interface"
    ;;               "class")
    ;;             " "
    ;;             file-name))
    ;;   \n "{"
    ;;   \n -
    ;;   \n "}")

    (define-abbrev-table 'csharp-tree-sitter-mode-abbrev-table
      '(
        ("cwl" "" jacob-insert-csharp-print)
        ("if" "" jacob-insert-c-if)
        ("pu" "public")
        ("pr" "private")
        ("as" "async")
        ("st" "static")
        ("ns" "namespace")
        ("meth" "" jacob-insert-java-method)
        ("guid" "Guid")
        ("var" "" jacob-insert-java-var)
        ("class" "" jacob-csharp-class)
        ("prop" "" jacob-insert-csharp-property)
        ))))


;; eglot config

(jacob-is-installed 'eglot
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
    
    (add-to-list 'eglot-server-programs '((web-mode js-mode typescript-mode) . ("typescript-language-server" "--stdio")))

    ;; (add-to-list 'eglot-server-programs '((js-mode typescript-mode) . (eglot-deno "deno" "lsp")))

    (defclass eglot-deno (eglot-lsp-server) ()
      :documentation "A custom class for deno lsp.")

    (cl-defmethod eglot-initialization-options ((server eglot-deno))
      "Passes through required deno initialization options"
      (list :enable t
            :lint t))
    
    (add-to-list 'eglot-server-programs '(go-mode . ("/home/jacob/go/bin/gopls")))

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
    (when (boundp 'purescript-mode-abbrev-table)
      (clear-abbrev-table purescript-mode-abbrev-table))

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
      > "listOf(" - ")")
    
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
        ("test" "" jacob-kotlin-test)))))

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


;; typescript config

(jacob-is-installed 'typescript-mode

  (put 'tsi-typescript-indent-offset 'safe-local-variable #'numberp)

  (define-derived-mode typescript-react-mode typescript-mode
    "Typescript TSX")

  (add-to-list 'auto-mode-alist '("\\.tsx?\\'" . typescript-react-mode))
  (with-eval-after-load 'tree-sitter
    (add-to-list 'tree-sitter-major-mode-language-alist '(typescript-react-mode . tsx))

    (jacob-try-require 'tsi
      (jacob-try-require 'tsi-typescript
        (add-hook 'typescript-mode-hook (lambda () (tsi-typescript-mode 1)))
        (add-hook 'typescript-react-mode-hook (lambda () (tsi-typescript-mode 1))))))

  (with-eval-after-load 'typescript-mode

    (jacob-js-config-hook-function)

    (when (boundp 'typescript-mode-abbrev-table)
      (clear-abbrev-table typescript-mode-abbrev-table))
    
    (define-abbrev-table 'typescript-mode-abbrev-table
      '(
        ("cl" "" jacob-insert-js-print)
        ("if" "" jacob-insert-c-if)
        ("while" "" jacob-insert-c-while)
        ("fun" "" jacob-insert-js-function)
        ("con" "" jacob-insert-js-const)
        ("let" "" jacob-insert-js-let)
        ("ret" "return")
        ))))



(jacob-is-installed 'web-mode
  (defun jacob-web-mode-config ()
    
    (if (string= (file-name-extension (buffer-name)) "tsx")
        (eglot-ensure))
    (setq-local electric-pair-pairs '((?\" . ?\") (?\< . ?\>))))

  (add-hook 'web-mode-hook 'jacob-web-mode-config)

  (add-to-list 'auto-mode-alist '("\\.cshtml\\'" . web-mode))
  ;; (add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
  
  (with-eval-after-load 'web-mode
    (add-to-list 'web-mode-engines-alist '("razor" . "\\.cshtml\\'"))
    (setq web-mode-markup-indent-offset 2)
    (setq web-mode-css-indent-offset 2)
    (setq web-mode-code-indent-offset 2)

    (jacob-js-config-hook-function)

    (when (boundp 'web-mode-abbrev-table)
      (clear-abbrev-table web-mode-abbrev-table))

    (define-abbrev-table 'web-mode-abbrev-table
      '(
        ("cl" "" jacob-js-skeleton-console-log)
        ("if" "" jacob-js-skeleton-if)
        ("arr" "" jacob-js-skeleton-arrow-function)
        ("con" "" jacob-js-skeleton-const)
        ("let" "" jacob-js-skeleton-let)
        ("fun" "" jacob-js-skeleton-arrow-function)
        ))))


;; tree sitter config

(jacob-is-installed 'tree-sitter
  (add-hook 'typescript-react-mode-hook (lambda () (global-tree-sitter-mode 1))))


;; xah-fly-keys config

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
      (xah-end-of-line-or-block))))

(provide 'jacob-package-configuration)

;;; jacob-package-configuration.el ends here
