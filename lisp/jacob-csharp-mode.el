;;; jacob-csharp-mode.el --- configuration for `csharp-mode'  -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:

;; JACOBTODO: follow-mode causes strange behaviour
(defun csharp-toolbox-format-statement ()
  "Format the csharp statement at point."
  (interactive)
  (let* ((get-statement-bounds (lambda ()
                                 (let ((statement-node (csharp-toolbox--get-parent-node "statement")))
                                   (cons (treesit-node-start statement-node)
                                         (treesit-node-end statement-node)))))
         (node-start (car (funcall get-statement-bounds))))
    (save-excursion
      (let* ((node-end (cdr (funcall get-statement-bounds)))
             (source-text (buffer-substring-no-properties node-start node-end)))
        (shell-command-on-region node-start
                                 node-end
                                 (format "csharpier-wrapper %s %d"
                                         (shell-quote-argument source-text)
                                         (save-excursion
                                           (goto-char node-start)
                                           (- 100
                                              (- (point)
                                                 (progn
                                                   (beginning-of-line)
                                                   (point))))))
                                 nil
                                 'no-mark)
        (goto-char node-start))
      (let* ((node-end (cdr (funcall get-statement-bounds)))
             (indentation-amount (save-excursion
                                   (goto-char node-start)
                                   (- (point)
                                      (progn
                                        (beginning-of-line)
                                        (point))))))
        (indent-code-rigidly node-start
                             node-end
                             indentation-amount)
        (goto-char node-start))
      (let* ((node-end (cdr (funcall get-statement-bounds))))
        (goto-char node-end)
        (delete-char 1)))))

;; JACOBTODO: follow-mode causes strange behaviour
;; JACOBTODO: reduce duplication
(defun csharp-toolbox-format-method ()
  "Format the csharp method at point."
  (interactive)
  (let* ((get-method-bounds (lambda ()
                              (let ((method-node (csharp-toolbox--get-parent-node "method_declaration")))
                                (cons (treesit-node-start method-node)
                                      (treesit-node-end method-node)))))
         (node-start (car (funcall get-method-bounds))))
    (save-excursion
      (let* ((node-end (cdr (funcall get-method-bounds)))
             (source-text (buffer-substring-no-properties node-start node-end)))
        (shell-command-on-region node-start
                                 node-end
                                 (format "csharpier-wrapper %s %d method"
                                         (shell-quote-argument source-text)
                                         (save-excursion
                                           (goto-char node-start)
                                           (- 100
                                              (- (point)
                                                 (progn
                                                   (beginning-of-line)
                                                   (point))))))
                                 nil
                                 'no-mark)
        (goto-char node-start))
      (let* ((node-end (cdr (funcall get-method-bounds)))
             (indentation-amount (save-excursion
                                   (goto-char node-start)
                                   (- (point)
                                      (progn
                                        (beginning-of-line)
                                        (point))))))
        (indent-code-rigidly node-start
                             node-end
                             indentation-amount)
        (goto-char node-start))
      (let* ((node-end (cdr (funcall get-method-bounds))))
        (goto-char node-end)
        (delete-char 1)))))

(defun csharp-toolbox-toggle-async ()
  "Toggle method at point async."
  (interactive)
  (let ((method-node (csharp-toolbox--get-method-node)))
    (if (null method-node)
        (message "method not found")
      (let* ((query-results (treesit-query-capture
                             method-node
                             '((method_declaration
                                (modifier "async") :? @async
                                [(predefined_type)
                                 (identifier)
                                 (generic_name (type_argument_list
                                                "<"
                                                :anchor
                                                (_) @wrapped-type
                                                :anchor
                                                ">"))]
                                @type
                                :anchor (identifier) :anchor (parameter_list) :anchor (block)))))
             (async-node (alist-get 'async query-results))
             (type-node (alist-get 'type query-results)))
        (save-excursion
          (cond ((and async-node (equal (treesit-node-text type-node) "Task"))
                 (goto-char (treesit-node-start async-node))
                 (search-forward "async Task")
                 (replace-match "void" "FIXEDCASE"))
                (async-node
                 (goto-char (treesit-node-start async-node))
                 (re-search-forward "async Task<.*>")
                 (replace-match (treesit-node-text (alist-get 'wrapped-type query-results))
                                "FIXEDCASE"))
                ;; no async modifier
                ((equal (treesit-node-text type-node) "void")
                 (goto-char (treesit-node-start type-node))
                 (delete-region (treesit-node-start type-node)
                                (treesit-node-end type-node))
                 (insert "async Task"))
                ((string-match-p "^Task" (treesit-node-text type-node))
                 (goto-char (treesit-node-start type-node))
                 (insert "async "))
                (t
                 (goto-char (treesit-node-start type-node))
                 (search-forward (treesit-node-text type-node))
                 (replace-match (format "async Task<%s>"
                                        (treesit-node-text type-node))
                                "FIXEDCASE"))))))))

(defun csharp-toolbox-highlight-statement ()
  "Select statement at point."
  (interactive)
  (let ((node (csharp-toolbox--get-parent-node "statement")))
    (goto-char (treesit-node-end node))
    (set-mark (treesit-node-start node))))

(defun csharp-toolbox-convert-namespace ()
  "Convert namespace block into namespace declaration."
  (interactive)
  (save-excursion
    (let ((namespace-declaration-node
           (treesit-parent-until (treesit-node-at (point))
                                 (lambda (node)
                                   (equal (treesit-node-type node)
                                          "namespace_declaration")))))
      (if (null namespace-declaration-node)
          (message "namespace declaration not found")
        (let* ((namespace-declaration-position
                (treesit-node-start namespace-declaration-node))
               (declaration-list-node
                (seq-find (lambda (node)
                            (equal (treesit-node-type node)
                                   "declaration_list"))
                          (treesit-node-children namespace-declaration-node)))
               (open-brace-position
                (treesit-node-start
                 (seq-first
                  (treesit-node-children
                   declaration-list-node)))))
          (goto-char open-brace-position)
          (forward-sexp)
          (delete-char -1)
          (goto-char open-brace-position)
          (delete-char 1)

          (goto-char namespace-declaration-position)
          (end-of-line)
          (insert ";")

          (forward-line 2)
          (indent-code-rigidly (point)
                               (buffer-end 1)
                               -4))))))

(defun csharp-toolbox-synchronise-related-methods ()
  "Synchronise signature of \"related\" methods with the method at point.

Related meaning the interface for the method and the other
implementations."
  (interactive)
  (let ((method-node
         (treesit-parent-until (treesit-node-at (point))
                               (lambda (node)
                                 (string-equal "method_declaration"
                                               (treesit-node-type node))))))
    (if (null method-node)
        (message "no method at point")
      (let ((parameters
             (treesit-node-text
              (seq-first
               (treesit-query-capture method-node
                                      '((parameter_list
                                         "("
                                         _
                                         @parameters
                                         ")"))
                                      nil
                                      nil
                                      "NODE-ONLY"))))
            (references
             (let ((children
                    (treesit-node-children method-node)))
               (goto-char (treesit-node-start
                           (seq-elt children
                                    (- (length children)
                                       3))))
               (let ((xref-show-xrefs-function
                      (lambda (fetcher alist)
                        (seq-map 'xref-item-location
                                 (funcall fetcher)))))
                 (xref-find-references
                  (xref-backend-identifier-at-point
                   'eglot-xref-backend))))))

        ;; replace their parameter lists
        (seq-do (lambda (reference)
                  (find-file (xref-file-location-file reference))
                  (goto-char (point-min))
                  (forward-line (xref-file-location-line reference))
                  ;; (forward-char (xref-file-location-column reference))
                  ;; i can't get the references to the other methods
                  ;; this function won't work ;_;
                  ;; but i learnt some stuff i guess
                  )
                references)))))

(defun csharp-toolbox--select-dll ()
  "Function to select dll for dape."
  (completing-read "dll: "
                   (seq-map (lambda (filename)
                              (cons (file-name-nondirectory filename)
                                    filename))
                            (directory-files-recursively
                             (project-root (project-current))
                             "\\.dll"))))

(defun csharp-toolbox-run-test (debug)
  "Run test at point.

Set the environment variable `VSTEST_HOST_DEBUG' to 1 if DEBUG is
non-nil."
  (interactive "P")
  (let* ((test-name
          (format "%s.%s.%s"
                  (treesit-node-text
                   (treesit-node-child-by-field-name
                    (csharp-toolbox--get-namespace-node)
                    "name")
                   "NO_PROPERTY")
                  (treesit-node-text
                   (csharp-toolbox--get-identifier-child
                    (csharp-toolbox--get-class-node))
                   "NO_PROPERTY")
                  (treesit-node-text
                   (csharp-toolbox--get-identifier-child
                    (csharp-toolbox--get-method-node))
                   "NO_PROPERTY")))
         (default-directory
          (locate-dominating-file
           (file-name-directory (buffer-file-name (current-buffer)))
           (lambda (dir)
             (not (seq-empty-p (seq-filter
                                (lambda (filename)
                                  (string-match-p "csproj" filename))
                                (directory-files dir))))))))
    (compile (format "VSTEST_HOST_DEBUG=%d dotnet test --filter \"FullyQualifiedName=%s\""
                     (if debug 1 0)
                     test-name))
    (with-current-buffer "*compilation*"
      (let ((process-id))
        (add-hook 'compilation-filter-hook
                  (lambda ()
                    (let ((inserted-text
                           (buffer-substring-no-properties compilation-filter-start
                                                           (point))))
                      (when (string-match-p "Process Id:" inserted-text)
                        (goto-char compilation-filter-start)
                        (search-forward "Process Id: ")
                        (setq process-id (thing-at-point 'word "NO_PROPERTIES"))
                        (let* ((dll
                                (csharp-toolbox--select-dll))
                               (config
                                `(modes (csharp-mode csharp-ts-mode)
                                        ensure dape-ensure-command
                                        command "netcoredbg"
                                        command-args ["--interpreter=vscode"]
                                        :request "attach"
                                        :cwd ,(funcall dape-cwd-fn)
                                        :program ,dll
                                        :stopAtEntry t
                                        :processId ,process-id)))
                          (dape config)))

                      ))
                  nil
                  "LOCAL")))))

(defun csharp-toolbox--get-namespace-node ()
  "Get the namespace node at point."
  (csharp-toolbox--get-parent-node "\\(file_scoped_\\)?namespace_declaration"))

(defun csharp-toolbox--get-class-node ()
  "Get the class node at point."
  (csharp-toolbox--get-parent-node "class_declaration"))

(defun csharp-toolbox--get-method-node ()
  "Get the method node at point."
  (csharp-toolbox--get-parent-node "method_declaration"))

(defun csharp-toolbox--get-parent-node (regexp)
  "Find dominating node at point that matches REGEXP."
  (treesit-parent-until (treesit-node-at (point))
                        (lambda (node)
                          (string-match-p regexp
                                          (treesit-node-type node)))
                        t))

(defun csharp-toolbox--get-identifier-child (node)
  "Get the identifier node child of NODE.

Useful for getting the name node of classes or methods."
  (car
   (last
    (treesit-filter-child node
                          (lambda (node)
                            (string-match "identifier"
                                          (treesit-node-type node)))))))

;; JACOBTODO: try make a function for creating fields and adding them to the constructor

(defun csharp-toolbox-rename-file ()
  "Rename the current file and class/whatever."
  (interactive)
  (let ((new-name (read-from-minibuffer "Rename window/class to: "))
        (class-name-range (seq-first (treesit-query-range (treesit-buffer-root-node)
                                                          '((interface_declaration (identifier) @identifier))))))
    (rename-visited-file (format "%s.cs" new-name))
    (delete-region (car class-name-range) (cdr class-name-range))
    (goto-char (car class-name-range))
    (insert new-name)))

(defun jacob-csharp-move-type ()
  "Move the type at point.

A type is a csharp class, record, interface or enum.

Move can mean:
- Rename the type and its containing file.
- Move a type out of a file that contains multiple types into its own file.

In all cases we should attempt to keep the only necessary usings
\(`using_directive'), and the correct namespace
\(`file_scoped_namespace_declaration')."
  (interactive)
  (let* ((type-node (treesit-parent-until (treesit-node-at (point))
                                          (lambda (node)
                                            "Find a class, record, enum or interface node.
TODO: can this be just the regex instead?"
                                            (string-match-p
                                             "\\(class\\|record\\|enum\\|interface\\)_declaration"
                                             (treesit-node-type node)))
                                          "INCLUDE_NODE"))
         (type-code (treesit-node-text type-node))
         (type-name (treesit-node-text
                     (car
                      (treesit-query-capture type-node
                                             '([(class_declaration name: (identifier) @identifier)
                                                (interface_declaration name: (identifier) @identifier)
                                                (enum_declaration name: (identifier) @identifier)
                                                (record_declaration name: (identifier) @identifier)])
                                             nil
                                             nil
                                             "NODE_ONLY"))
                     "NO_PROPERTIES"))
         (using-directives (string-join (seq-map #'treesit-node-text
                                                 (treesit-query-capture (treesit-buffer-root-node)
                                                                        '((compilation_unit (using_directive) @using))
                                                                        nil
                                                                        nil
                                                                        "NODE_ONLY"))
                                        "\n"))
         (target-directory (read-directory-name "Move file to: "))
         (target-file-name (file-name-concat target-directory
                                             (file-name-nondirectory
                                              (read-file-name (format "Moving to \"%s\". File name: "
                                                                      target-directory)
                                                              target-directory
                                                              nil
                                                              nil
                                                              (concat type-name
                                                                      ".cs"))))))
    (ignore-errors (make-directory target-directory))
    (save-window-excursion
      ;; TODO: make the mishmash of namespace, usings look nicer
      (find-file target-file-name)
      (insert using-directives)
      (insert "\n\n")
      (insert type-code)
      (jacob-csharp-fix-namespace))
    (delete-region (treesit-node-start type-node)
                   (treesit-node-end type-node))))

(defun jacob-csharp-fix-namespace ()
  "Fix the namespace of the current file."
  (interactive)
  (unless (treesit-query-capture (treesit-buffer-root-node)
                                 '((file_scoped_namespace_declaration (_) @x)))
    (let ((usings (treesit-query-capture (treesit-buffer-root-node)
                                         '((compilation_unit (using_directive) @using))
                                         nil nil "NODE_ONLY")))
      (goto-char (if (null usings)
                     (point-min)
                   (treesit-node-end (car (last usings)))))
      (insert "\n\nnamespace xxx;")))
  (let* ((guessed-namespace (jacob-csharp-determine-file-namespace))
         (current-namespace-range (car (treesit-query-range
                                        (treesit-buffer-root-node)
                                        '((file_scoped_namespace_declaration name: (_) @x))))))
    (delete-region (car current-namespace-range)
                   (cdr current-namespace-range))
    (save-excursion
      (goto-char (car current-namespace-range))
      (insert guessed-namespace))))

(defun jacob-csharp-determine-file-namespace ()
  "Return the namespace of the current file."
  (let* ((project-directory (locate-dominating-file (buffer-file-name)
                                                    (lambda (directory)
                                                      (seq-find (lambda (file)
                                                                  (string-match-p "\\.csproj$" file))
                                                                (directory-files directory)))))
         (relative-path (file-name-concat
                         (file-name-nondirectory (directory-file-name (file-name-directory project-directory)))
                         (file-relative-name (buffer-file-name) project-directory))))
    (string-replace "/" "." (directory-file-name (file-name-directory relative-path)))))

(defun jacob-csharp-move-file ()
  "Move the current file.

Update the class/record/interface name and the namespace to reflect the
new location and/or name of the file."
  (interactive)
  (let* ((new-file-name (read-file-name "Move file to: "
                                        nil
                                        buffer-file-name))
         (new-class-name (file-name-sans-extension
                          (file-name-nondirectory
                           new-file-name)))
         (class-name-range (car
                            (treesit-query-range (treesit-buffer-root-node)
                                                 '((class_declaration (identifier) @identifier))))))
    ;; 1. rename the file
    (rename-visited-file new-file-name)
    ;; 2. rename the class
    (save-excursion
      (delete-region (car class-name-range) (cdr class-name-range))
      (goto-char (car class-name-range))
      (insert new-class-name)))
  ;; 3. update the namespace
  (jacob-csharp-fix-namespace))

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

(defun jacob-csharp-mode-config ()
  "Configuration for `csharp-mode'."
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

             ((match "block" "lambda_expression") parent-bol 0)
             ((parent-is "lambda_expression") parent-bol csharp-ts-mode-indent-offset)

             ((parent-is "try_statement") parent-bol 0)
             ((parent-is "catch_clause") parent-bol 0)
             ((parent-is "record_declaration") parent-bol 0)
             ((parent-is "interface_declaration") parent-bol 0)
             ((parent-is "throw_expression") parent-bol csharp-ts-mode-indent-offset)
             ((parent-is "return_statement") parent-bol csharp-ts-mode-indent-offset)
             ((parent-is "record_declaration") parent-bol 0)
             ((parent-is "interface_declaration") parent-bol 0)
             ((parent-is "arrow_expression_clause") parent-bol csharp-ts-mode-indent-offset)
             ((parent-is "property_pattern_clause") parent-bol csharp-ts-mode-indent-offset))))

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

  (advice-add #'eglot--lsp-xrefs-for-method :filter-return #'eglot-csharp-ls-metadata))

(use-package csharp-mode
  :hook ((csharp-ts-mode-hook . apheleia-mode)
         (csharp-ts-mode-hook . yas-minor-mode)
         (csharp-ts-mode-hook . electric-indent-local-mode)
         (csharp-ts-mode-hook . jacob-trim-quotes-mode))
  :config
  (jacob-csharp-mode-config))

(provide 'jacob-csharp-mode)

;;; jacob-csharp-mode.el ends here
