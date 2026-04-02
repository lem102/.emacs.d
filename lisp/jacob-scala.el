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

(defun jacob-scala--find-nearest-scala-file (file)
  "Find the nearest scala file to FILE."
  (seq-find (lambda (f)
              (string-match-p "\\.scala" f))
            (directory-files (file-name-directory file))))

;; TODO: improve this.
(defun jacob-scala-package ()
  "Return the package of the current scala file."
  (if-let* ((file (buffer-file-name (current-buffer)))
            (nearest-file (jacob-scala--find-nearest-scala-file file)))
      (jacob-scala--package nearest-file)
    (string-replace "/"
                    "."
                    (file-relative-name (directory-file-name (file-name-directory file))
                                        (project-root (project-current))))))

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

(provide 'jacob-scala)

;;; jacob-scala.el ends here
