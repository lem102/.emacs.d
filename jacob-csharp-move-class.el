;;; jacob-csharp-move-type --- Summary:

;;; commentary:

;;; code:

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

(defun jacob-csharp-move-class ()
  "Move the class, record or interface at point to a new file."
  (interactive)
  (let* ((declaration-node (treesit-parent-until (treesit-node-at (point))
                                                 (lambda (node)
                                                   "Find a class, record, enum or interface node."
                                                   (string-match-p
                                                    "\\(class\\|record\\|enum\\|interface\\)_declaration"
                                                    (treesit-node-type node)))
                                                 t))
         (declaration-code (treesit-node-text declaration-node))
         (declaration-name (treesit-node-text
                            (car
                             (treesit-query-capture declaration-node
                                                    '((record_declaration name: (identifier) @identifier))
                                                    nil
                                                    nil
                                                    "NODE_ONLY"))
                            "NO_PROPERTIES"))
         (new-file-name (read-file-name "Move file to: "
                                        nil
                                        nil
                                        nil
                                        (concat declaration-name
                                                ".cs"))))
    (ignore-errors
      (make-directory (file-name-directory new-file-name "PARENTS")))
    (save-window-excursion
      (find-file new-file-name)
      (insert declaration-code)
      (jacob-csharp-fix-namespace))
    (delete-region (treesit-node-start declaration-node)
                   (treesit-node-end declaration-node))))

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


;;; jacob-csharp-move-class.el ends here
