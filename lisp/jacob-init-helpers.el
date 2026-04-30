;;; jacob-init-helpers.el --- Helpers for init files  -*- lexical-binding: t; -*-


;;; Commentary:
;; 

;;; Code:

(defmacro jacob-defhookf (hook &rest body)
  "Define function with BODY and bind it to HOOK."
  (declare (indent defun))
  (let* ((hook-name (symbol-name hook))
         (function-name (intern (concat "jacob-" hook-name "-function"))))
    `(progn
       (defun ,function-name ()
         ,(format "Auto-generated hook function for `%s'." hook-name)
         ,@body)
       (add-hook ',hook #',function-name))))

(defun jacob-format-custom-file (&rest _args)
  "Modify custom file so `package-selected-packages' is one per line."
  (with-temp-file custom-file
    (switch-to-buffer (current-buffer))
    (insert-file-contents custom-file)
    (goto-char (point-min))
    (search-forward "'(package-selected-packages")
    (forward-sexp)
    (backward-sexp)
    (down-list)
    (ignore-errors
      (while t
        (forward-sexp)
        (lisp-indent-line)
        (unless (= (char-after) (string-to-char "\n"))
          (insert "\n"))))))

(defun jacob-autoloads-generate ()
  "Generate autoloads file for Emacs configuration."
  (interactive)
  (let* ((autoloads-file (file-name-concat jacob-lisp-directory "jacob-autoloads.el")))
    (loaddefs-generate jacob-lisp-directory
                       autoloads-file
                       nil
                       nil
                       nil
                       "GENERATE-FULL")
    (with-temp-file autoloads-file
      (insert-file-contents autoloads-file)
      (goto-char (point-min))
      (when (search-forward ";; This file is part of GNU Emacs."
                            nil
                            "NOERROR")
        (kill-whole-line 2)))))

(provide 'jacob-init-helpers)

;;; jacob-init-helpers.el ends here
