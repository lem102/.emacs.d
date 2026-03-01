;;; jacob-csharp-move-type --- Summary:

;;; commentary:

;;; code:

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

;;; jacob-csharp-move-class.el ends here
