;;; jacob-scala-package.el --- Work out a package for the current scala file  -*- lexical-binding: t; -*-

;;; Commentary:

;; It seems that there is no standard way of naming packages.

;; Idea 1: Steal the package name from a nearby file.
;; - Simple when there is a scala file in the current directory.
;; - Complex when we have no files in the current directory. Do we go up
;;   a directory to try find new files?

;; Idea 2: Determine which part of the directory structure can be discarded
;; by comparing an existing package name with the directory structure.

;; Rough plan:

;; Step 1: find the nearest scala file.
;; Step 2: get its package
;; Step 3a: reuse the package for the current scala file (when nearest is
;; in same directory)
;; Step 3b: adjust the package for the current scala file (when nearest
;; is in higher directory)

;;; Code:

(defun jacob-scala-package--file-package (file)
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

(defun jacob-scala-package--get-nearest-scala-file (file)
  "Find the nearest scala file to FILE."
  (seq-find (lambda (f)
              (string-match-p "\\.scala" f))
            (directory-files (file-name-directory file))))

(defun jacob-scala-package ()
  "Return the package of the current scala file."
  (if-let* ((file (buffer-file-name (current-buffer)))
            (nearest-file (jacob-scala-package--get-nearest-scala-file file)))
      (jacob-scala-package--file-package nearest-file)
    (directory-file-name (file-name-directory file))))

(provide 'jacob-scala-package)

;;; jacob-scala-package.el ends here
