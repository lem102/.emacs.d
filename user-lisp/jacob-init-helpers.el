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

;;;###autoload
(defun jacob-disable-auto-save-in-buffer ()
  "Disable `auto-save-visited-mode' locally."
  (setq-local auto-save-visited-mode nil))

;; first minibuffer use hook

(defvar jacob-first-minibuffer-use-hook '()
  "Hook for the first time the minibuffer is used.")

(defun jacob-first-minibuffer-use-run-hook (&rest _args)
  "Run `jacob-first-minibuffer-use-hook';
then remove this function from `find-file-hook'."
  (when (member 'init features)
    (run-hooks 'jacob-first-minibuffer-use-hook)
    (advice-remove #'completing-read
                   #'jacob-first-minibuffer-use-run-hook)))

(provide 'jacob-init-helpers)

;;; jacob-init-helpers.el ends here
