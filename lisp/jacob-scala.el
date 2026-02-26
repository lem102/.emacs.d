;;; jacob-scala.el --- Utilities for scala  -*- lexical-binding: t; -*-

;;; Commentary:
;; 

;;; Code:

;;;###autoload
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

;;;###autoload
(defun jacob-scala-test-file ()
  "Test the current file."
  (interactive)
  (let ((package (treesit-node-text
                  (car
                   (treesit-query-capture (treesit-buffer-root-node)
                                          '((package_clause name: (package_identifier (identifier) @x)))
                                          nil
                                          nil
                                          "NODE_ONLY"))))
        (class (treesit-node-text
                (car
                 (treesit-query-capture (treesit-buffer-root-node)
                                        '((class_definition name: (identifier) @x))
                                        nil
                                        nil
                                        "NODE_ONLY"))))
        (default-directory (project-root (project-current))))
    (compile (format "sbt \"testOnly %s.%s\"" package class))))

;;;###autoload
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

(defun jacob-scala--get-file-package (file)
  "Get FILE's package."
  (with-temp-buffer
    (insert-file-contents file)
    (treesit-node-text
     (seq-first
      (treesit-query-capture (treesit-buffer-root-node 'scala)
                             '((package_clause (package_identifier) @package))
                             nil
                             nil
                             "NODE-ONLY")))))

(defun jacob-scala--find-nearest-scala-file (file)
  "Find the nearest scala file to FILE."
  (seq-find (lambda (f)
              (string-match-p "\\.scala" f))
            (directory-files (file-name-directory file))))

;;;###autoload
(defun jacob-scala-package ()
  "Return the package of the current scala file."
  (if-let* ((file (buffer-file-name (current-buffer)))
            (nearest-file (jacob-scala--find-nearest-scala-file file)))
      (jacob-scala--get-file-package nearest-file)
    (directory-file-name (file-name-directory file))))

;;;###autoload
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

(provide 'jacob-scala)

;;; jacob-scala.el ends here
