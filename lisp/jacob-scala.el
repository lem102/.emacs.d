;;; jacob-scala.el --- Utilities for scala  -*- lexical-binding: t; -*-

;;; Commentary:
;;

;; TODO: a comprehensive solution for renaming/moving files in scala.
;; needs to update filenames, and update packages around the project
;; so that after the rename the package is updated and there are no
;; compilation errors.

;; it should be possible to rename a directory, and then have the
;; packages around the project be updated accordingly.

;;; Code:

(defun jacob-project-sbt ()
  "Open sbt shell for a project."
  (interactive)
  (let* ((default-directory (project-root (project-current "MAYBE-PROMPT"))))
    (sbt-start)))

(defun jacob-scala-indentation-to-block ()
  "Convert the indentation based syntax at point to block based syntax."
  (interactive)
  (save-excursion
    (cond-let ([colon-node (treesit-parent-until (treesit-node-at (point)) "colon_argument")]
               (unless colon-node
                 (user-error "Cannot find colon"))
               (goto-char (treesit-node-end colon-node))
               (insert "}")
               (goto-char (treesit-node-start colon-node))
               (search-backward ":")
               (delete-char 1)
               (insert "{"))
              ([indented-cases-node (treesit-parent-until (treesit-node-at (point)) "indented_\\(cases\\|block\\)")]
               (goto-char (treesit-node-end indented-cases-node))
               (insert "}")
               (goto-char (treesit-node-start indented-cases-node))
               (insert "{")))))

(defun jscala-test-file ()
  "Test the current file."
  (interactive)
  (let* ((file (if (jscala-test-file-p)
                   (buffer-file-name (current-buffer))
                 (jscala-find-test-file)))
         (package (jscala--package file))
         (class (with-temp-buffer
                  (insert-file-contents file)
                  (treesit-node-text
                   (car
                    (treesit-query-capture (treesit-buffer-root-node 'scala)
                                           '((class_definition name: (identifier) @x))
                                           nil
                                           nil
                                           "NODE_ONLY")))))
         (default-directory (project-root (project-current))))
    (sbt-command (format "testOnly %s.%s" package class))))

(defun jacob-scala-dollar ()
  "Insert a dollar. If inside a string, enable string interpolation."
  (interactive)
  (unless (or (eq major-mode 'scala-mode)
              (eq major-mode 'scala-ts-mode))
    (user-error "Not in a `scala-ts-mode' buffer"))
  (insert "$")
  (let* ((string-node (treesit-parent-until (treesit-node-at (point) 'scala)
                                            "string"
                                            "INCLUDE-NODE"))
         (interpolated-string-node (treesit-parent-until string-node
                                                         "interpolated_string"
                                                         "INCLUDE-NODE"))
         (is-raw-string (string-match-p "^\"\"\".+\"\"\"" (treesit-node-text string-node))))
    (when (and string-node
               (not interpolated-string-node)
               (not is-raw-string))
      (save-excursion
        (goto-char (treesit-node-start string-node))
        (insert "s")))))

(defun jacob-scala-. ()
  "Insert a dot.

If inside a string using string interpolation and to the right of a value to be interpolated into the string, add curly braces appropriately."
  (interactive)
  (unless (or (eq major-mode 'scala-mode)
              (eq major-mode 'scala-ts-mode))
    (user-error "Not in a `scala-ts-mode' buffer"))
  (insert ".")
  (let* ((interpolated-string-node (treesit-parent-until (treesit-node-at (point))
                                                         "interpolated_string"
                                                         "INCLUDE-NODE"))
         (interpolation-node (treesit-parent-until (treesit-node-at (point))
                                                   "interpolation"
                                                   "INCLUDE-NODE")))
    (when (and interpolated-string-node
               (not interpolation-node))
      (save-excursion
        (insert "}")
        (search-backward "$")
        (forward-char 1)
        (insert "{")))))

(defun jscala--package (&optional buffer-or-file)
  "Get the current package of BUFFER-OR-FILE.

When BUFFER-OR-FILE is:
- nil, query the current buffer.
- a buffer, query the buffer.
- a file, query the file."
  (let ((parse-package-from-buffer
         (lambda ()
           (treesit-node-text
            (seq-first
             (treesit-query-capture (treesit-buffer-root-node 'scala)
                                    '((package_clause (package_identifier) @package))
                                    nil
                                    nil
                                    "NODE-ONLY"))
            "NO_PROPERTY"))))
    (cond ((null buffer-or-file)
           (funcall parse-package-from-buffer))
          ((bufferp buffer-or-file)
           (with-current-buffer buffer-or-file
             (funcall parse-package-from-buffer)))
          ((file-readable-p buffer-or-file)
           (with-temp-buffer
             (insert-file-contents buffer-or-file)
             (funcall parse-package-from-buffer))))))

(defun jscala-test-file-p (&optional buffer-or-file)
  "Return t if BUFFER-OR-FILE corresponds to a scala test file.

When BUFFER-OR-FILE is:
- nil, check the current buffer;
- a buffer, check the buffer;
- a file, check the file."
  (string-match-p "\\Spec.scala$"
                  (cond ((null buffer-or-file) (buffer-file-name (current-buffer)))
                        ((bufferp buffer-or-file) (buffer-file-name buffer-or-file))
                        ((file-readable-p buffer-or-file) buffer-or-file)
                        (t (user-error "Invalid argument to jacob-scala-test-file-p")))))

(defun jscala-find-test-file (&optional buffer-or-file)
  "Return the filename of the test file that corresponds to BUFFER-OR-FILE.

When BUFFER-OR-FILE is:
- nil, find the test file that corresponds to the current buffer;
- a buffer, find the test file that corresponds to the buffer;
- a file, find the test file that corresponds to the file.

If BUFFER-OR-FILE already corresponds to a test file, return the
filename equivalent of BUFFER-OR-FILE."
  (let* ((target (cond ((null buffer-or-file) (buffer-file-name (current-buffer)))
                       ((bufferp buffer-or-file) (buffer-file-name (current-buffer)))
                       ((file-readable-p buffer-or-file) buffer-or-file)
                       (t (user-error "Invalid argument to jacob-scala-find-test-file")))))
    (seq-find (lambda (f)
                (string= (file-name-nondirectory f)
                         (if (jscala-test-file-p buffer-or-file)
                             (file-name-nondirectory target)
                           (format "%s.%s"
                                   (concat (file-name-sans-extension (file-name-nondirectory target)) "Spec")
                                   (file-name-extension target)))))
              (project-files (project-current)))))

(defun jacob-scala-calculate-package (file)
  "Calculate the package of FILE based on the directory."
  (let* ((relative-roots '("app" "test" "it/test" "src/test/scala"))
         (sbt-root (locate-dominating-file file "build.sbt"))
         (absolute-roots (seq-map (lambda (root)
                                    "Append ROOT to the sbt-root."
                                    (file-name-concat sbt-root root))
                                  relative-roots))
         (absolute-root (seq-find (lambda (root)
                                    "Return t if file belongs to ROOT."
                                    (file-in-directory-p file root))
                                  absolute-roots))
         (relative-filepath (file-relative-name file absolute-root))
         (directory (directory-file-name (file-name-directory relative-filepath)))
         (package (string-replace "/" "." directory)))
    package))

(defun jacob-scala-fix-package (file)
  "Fix the package of the scala file FILE.

Interactively, fix the current buffer's package."
  (interactive (list (buffer-file-name)))
  (with-temp-file file
    (insert-file-contents file)
    (when-let* ((calculated-package (jacob-scala-calculate-package file))
                (package-identifier-bounds (seq-first
                                            (treesit-query-range
                                             (treesit-parser-root-node (treesit-parser-create 'scala (current-buffer)))
                                             '((package_identifier (identifier) @x))))))
      (save-excursion
        (delete-region (car package-identifier-bounds)
                       (cdr package-identifier-bounds))
        (goto-char (car package-identifier-bounds))
        (insert calculated-package)))))

;; TODO: figure out more automatic method of fixing packages on the fly
;; metals should be doing this for us, needs a change in eglot
(defun jacob-scala-fix-packages-in-project ()
  "Fixup all the packages in the current project."
  (interactive)
  (let* ((files (directory-files-recursively (project-root (project-current))
                                             ".scala$"
                                             nil
                                             (lambda (subdir)
                                               (not (seq-contains-p '(".g8" ".metals")
                                                                    (file-name-nondirectory subdir)))))))
    (seq-do #'jacob-scala-fix-package files)))

(defun jacob-scala-toggle-raw-string ()
  "Convert strings to raw strings and vice versa.

Leave escaped characters alone."
  (interactive)
  (save-excursion
    (let* ((string-node (treesit-parent-until (treesit-node-at (point))
                                              "string"
                                              "INCLUDE-NODE"))
           (is-raw-string (string-match-p "^\"\"\".+\"\"\"" (treesit-node-text string-node))))
      (if is-raw-string
          (progn
            (goto-char (treesit-node-end string-node))
            (search-backward "\"\"\"")
            (delete-forward-char 2)
            (goto-char (treesit-node-start string-node))
            (search-forward "\"\"\"")
            (backward-delete-char 2))
        (goto-char (treesit-node-end string-node))
        (search-backward "\"")
        (insert "\"\"")
        (goto-char (treesit-node-start string-node))
        (search-forward "\"")
        (insert "\"\"")))))

(defun jacob-scala-font-lock-setup ()
  "Setup faces locally for scala."
  (setq-local treesit-font-lock-feature-list
              '((comment doc-comment definition)
                (type)
                (import extra)
                (function operator literal interpolation)))
  (treesit-font-lock-recompute-features)
  (face-remap-add-relative 'font-lock-comment-face
                           :inherit 'font-lock-warning-face))

(defun jacob-scala-steal-import ()
  "Try to import the symbol at point using grep.
Finds existing import statements for the symbol in the current project,
lets you select one via completion, and inserts it at the top of the file."
  (interactive)
  (let* ((symbol (thing-at-point 'symbol t))
         (project (project-current))
         (root (project-root project))
         (search-regexp (concat "^import .*" (regexp-quote symbol)))
         (matches (when symbol
                    (split-string (shell-command-to-string
                                   (format "grep --recursive --no-filename --include=\\*scala --max-count 1 \"%s\" %s"
                                           search-regexp
                                           root))
                                  "\n"
                                  "OMIT-NULLS")))
         (import (car-safe matches)))
    (if (null import)
        (message "Import failed.")
      (save-excursion
        (goto-char (point-min))
        (when (re-search-forward "^package .*" nil t)
          (forward-line 1))
        (insert import "\n")
        (message "Nicked import: %s" import)))))

(defun jacob-scala-switch-build-server ()
  "Switch metals build server."
  (interactive)
  (jsonrpc-notify (eglot-current-server)
                  :workspace/executeCommand
                  '( :command "bsp-switch")))

(defun jacob-json-to-play-json (region-start region-end)
  "Convert the json between REGION-START and REGION-END to an equivalent play json expression."
  (interactive "r")
  (let* ((json (buffer-substring-no-properties region-start region-end))
         (data (condition-case nil
                   (json-parse-string json
                                      :object-type 'plist)
                 (error (user-error "Region does not contain valid json"))))
         (play-json-code (cl-labels ((traverse-outer (v)
                                       (pcase v
                                         ((pred plistp)
                                          (concat "Json.obj(" (traverse-inner v) ")"))
                                         ((pred vectorp)
                                          (concat "Json.arr(" (traverse-inner (append v nil)) ")"))))
                                     (traverse-inner (v)
                                       (pcase (car v)
                                         ((pred null)
                                          "")
                                         ((pred keywordp)
                                          (format "\"%s\" -> %s"
                                                  (seq-rest (symbol-name (car v)))
                                                  (traverse-inner (cdr v))))
                                         ((pred stringp)
                                          (format "\"%s\", %s"
                                                  (car v)
                                                  (traverse-inner (cdr v))))
                                         ((pred numberp)
                                          (format "%s, %s"
                                                  (car v)
                                                  (traverse-inner (cdr v))))
                                         ((or (pred listp)
                                              (pred vectorp))
                                          (format "%s, %s"
                                                  (traverse-outer (car v))
                                                  (traverse-inner (cdr v)))))))
                           (traverse-outer data))))
    (delete-region region-start region-end)
    (insert play-json-code)))

;;; edit json string

(defvar jacob-scala-edit-json-string-node nil
  "The treesit node currently being edited in a json buffer.")

(defun jacob-scala-edit-json-string ()
  "Display a json buffer with the contents of the string at point in."
  (interactive)
  (let* ((string-node (car (treesit-query-capture (treesit-node-at (point))
                                                  '(([(string) (interpolated_string)] @string))
                                                  nil
                                                  nil
                                                  "NODE-ONLY")))
         (buffer (get-buffer-create "*jacob-scala-edit-json*")))
    (with-current-buffer buffer
      (delete-region (point-min) (point-max))
      (insert (string-trim (treesit-node-text string-node) "\"\"\"" "\"\"\""))
      (goto-char (point-min))
      ;; TODO: handle strings with no strip margin | characters
      (while (re-search-forward " +|" nil "NOERROR")
        (delete-region (match-beginning 0)
                       (match-end 0)))
      (json-ts-mode))
    (setq jacob-scala-edit-json-string-node string-node)
    (display-buffer buffer)))

(defun jacob-scala-edit-json-string-apply ()
  "Apply the string in the json editing buffer back to the original location."
  (interactive)
  (unless (string= "*jacob-scala-edit-json*" (buffer-name (current-buffer)))
    (user-error "Not in correct buffer"))
  (unless jacob-scala-edit-json-string-node
    (user-error "String node not found"))
  (let* ((new-text (buffer-string))
         (string-node jacob-scala-edit-json-string-node)
         (original-buffer (treesit-node-buffer string-node))
         (start (treesit-node-start string-node))
         (column (+ (with-current-buffer original-buffer
                      (save-excursion
                        (goto-char start)
                        (current-column)))
                    2))
         (end (treesit-node-end string-node)))
    (pop-to-buffer original-buffer)
    (delete-region start end)
    (goto-char start)
    (insert "\"\"\"")
    (let* ((lines (string-split new-text "\n"))
           (first-line (car lines))
           (last-line (car (last lines)))
           (rest-lines (seq-subseq lines 1 -1)))
      (insert first-line "\n")
      (dolist (line rest-lines)
        (dotimes (i column)
          (insert " "))
        (insert "|")
        (insert line)
        (insert "\n"))
      (dotimes (i column)
        (insert " "))
      (insert "|" last-line "\"\"\""))))

;; flymake

;; TODO: change this so it only applies when there is one class/object
;; pair in a file. when their are more pairs check that the class and
;; object are adjacent.
(defun jacob-flymake-scala-filename-alignment (report-fn &rest _args)
  "Check for classes that differ to their filename.

This is a flymake backend, hence it uses REPORT-FN to report diagnostics."
  (funcall report-fn (when-let* ((nodes (treesit-query-capture (treesit-buffer-root-node)
                                                               '([(class_definition name: (_) @identifier)
                                                                  (object_definition name: (_) @identifier)])
                                                               nil
                                                               nil
                                                               "NODE-ONLY"))
                                 (file-name (file-name-sans-extension (file-name-nondirectory buffer-file-name))))
                       (seq-keep (lambda (node)
                                   "If the NODE text does not match the file name, return a flymake diagnostic."
                                   (unless (string= (treesit-node-text node) file-name)
                                     (flymake-make-diagnostic (current-buffer)
                                                              (treesit-node-start node)
                                                              (treesit-node-end node)
                                                              :note
                                                              (format "Identifier %s does not match filename %s"
                                                                      (treesit-node-text node)
                                                                      file-name))))
                                 nodes))))

(defun jacob-flymake-scala-use-identity (report-fn &rest _args)
  "Check for anonymous functions that could be replaced by identity.

This is a flymake backend, hence it uses REPORT-FN to report diagnostics."
  (funcall report-fn (when-let* ((nodes (treesit-query-capture (treesit-buffer-root-node)
                                                               '((lambda_expression parameters:
                                                                                    (identifier) @x
                                                                                    "=>"
                                                                                    (identifier) @y))
                                                               nil
                                                               nil
                                                               "NODE-ONLY"))
                                 (node-pairs (seq-partition nodes 2)))
                       (seq-keep (lambda (node-pair)
                                   "If the NODE-PAIR both have the same text, return a flymake diagnostic."
                                   (let* ((node1 (seq-elt node-pair 0))
                                          (node2 (seq-elt node-pair 1)))
                                     (when (string= (treesit-node-text node1) (treesit-node-text node2))
                                       (flymake-make-diagnostic (current-buffer)
                                                                (treesit-node-start node1)
                                                                (treesit-node-end node2)
                                                                :note
                                                                "Can be replaced with the identity function"))))
                                 node-pairs))))

(defun jacob-flymake-scala-prefer-nil (report-fn &rest _args)
  "Check for usage of list constructor instead of Nil.

This is a flymake backend, hence it uses REPORT-FN to report diagnostics."
  (let* (diags)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "List()" nil "NOERROR")
        (push (flymake-make-diagnostic (current-buffer)
                                       (match-beginning 0)
                                       (match-end 0)
                                       :note
                                       "Prefer Nil")
              diags)))
    (funcall report-fn diags)))

(defun jacob-flymake-scala-directory-alignment (report-fn &rest _args)
  "Check for packages that don't match the directory structure.

This is a flymake backend, hence it uses REPORT-FN to report diagnostics."
  (funcall report-fn
           (when-let* ((directory-structure-package (jacob-scala-calculate-package buffer-file-name))
                       (node (car-safe (treesit-query-capture (treesit-buffer-root-node 'scala)
                                                              '((package_clause (package_identifier) @package))
                                                              nil
                                                              nil
                                                              "NODE-ONLY"))))
             (unless (string= (treesit-node-text node) directory-structure-package)
               (list (flymake-make-diagnostic (current-buffer)
                                              (treesit-node-start node)
                                              (treesit-node-end node)
                                              :note
                                              "Package does not match directory structure"))))))

(defun jacob-scala-setup-flymake ()
  "Activate flymake diagnostics functions."
  (add-hook 'flymake-diagnostic-functions #'jacob-flymake-scala-filename-alignment nil "LOCAL")
  (add-hook 'flymake-diagnostic-functions #'jacob-flymake-scala-directory-alignment nil "LOCAL")
  (add-hook 'flymake-diagnostic-functions #'jacob-flymake-scala-use-identity nil "LOCAL")
  (add-hook 'flymake-diagnostic-functions #'jacob-flymake-scala-prefer-nil nil "LOCAL"))

(provide 'jacob-scala)

;;; jacob-scala.el ends here

;; Local Variables:
;; read-symbol-shorthands: (("jscala-" . "jacob-scala-"))
;; End:
