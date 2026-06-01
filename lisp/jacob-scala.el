;;; jacob-scala.el --- Utilities for scala  -*- lexical-binding: t; -*-

;;; Commentary:
;; 

;;; Code:

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

(defun jacob-scala-test-file ()
  "Test the current file."
  (interactive)
  (let ((package (jacob-scala--package (buffer-file-name (current-buffer))))
        (class (treesit-node-text
                (car
                 (treesit-query-capture (treesit-buffer-root-node)
                                        '((class_definition name: (identifier) @x))
                                        nil
                                        nil
                                        "NODE_ONLY"))))
        (default-directory (project-root (project-current))))
    (sbt-command (format "testOnly %s.%s" package class))))

(defun jacob-scala-dollar ()
  "Insert a dollar. If inside a string, enable string interpolation."
  (interactive)
  (unless (eq major-mode 'scala-ts-mode)
    (user-error "Not in a `scala-ts-mode' buffer"))
  (insert "$")
  (let* ((string-node (treesit-parent-until (treesit-node-at (point))
                                            "string"
                                            "INCLUDE-NODE"))
         (interpolated-string-node (treesit-parent-until string-node
                                                         "interpolated_string"
                                                         "INCLUDE-NODE")))
    (when (and string-node
               (not interpolated-string-node))
      (save-excursion
        (goto-char (treesit-node-start string-node))
        (insert "s")))))

;; TODO: combine this with `jacob-scala-package'
(defun jacob-scala--package (&optional buffer-or-file)
  "Get the package of BUFFER-OR-FILE.

When BUFFER-OR-FILE is nil, query the current buffer.
When BUFFER-OR-FILE is a buffer, query the buffer.
When BUFFER-OR-FILE is a file, query the file."
  (let ((f (lambda ()
             (treesit-node-text
              (seq-first
               (treesit-query-capture (treesit-buffer-root-node 'scala)
                                      '((package_clause (package_identifier) @package))
                                      nil
                                      nil
                                      "NODE-ONLY"))
              "NO_PROPERTY"))))
    (cond ((null buffer-or-file)
           (funcall f))
          ((bufferp buffer-or-file)
           (with-current-buffer buffer-or-file
             (funcall f)))
          ((file-readable-p buffer-or-file)
           (with-temp-buffer
             (insert-file-contents buffer-or-file)
             (funcall f))))))

(defun jacob-scala-calculate-package (file)
  "Calculate the package of FILE."
  (let* ((sbt-root (locate-dominating-file file "build.sbt"))
         (is-play-framework (file-exists-p (file-name-concat sbt-root "/conf/application.conf"))))

    (unless is-play-framework
      ;; TODO: extend this to work with "regular" scala projects
      (user-error "Not a play framework project"))

    (let* ((app-root (file-name-concat sbt-root "app"))
           (test-root (file-name-concat sbt-root "test"))
           (root (if (file-in-directory-p file app-root)
                     app-root
                   test-root))
           (relative-filepath (file-relative-name file root))
           (directory (directory-file-name (file-name-directory relative-filepath)))
           (package (string-replace "/" "." directory)))
      package)))

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
        (insert "\"\""))
      )))

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

(provide 'jacob-scala)

;;; jacob-scala.el ends here
