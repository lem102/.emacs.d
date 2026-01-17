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

(provide 'jacob-init-helpers)

;;; jacob-init-helpers.el ends here
