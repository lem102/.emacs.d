;;; jacob-use-package.el --- Utilities for use-package  -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:

(require 'use-package-core)
(require 'use-package-ensure)

(defalias 'use-package-normalize/:jacob-ensure-safely 'use-package-normalize/:ensure)

(defun use-package-handler/:jacob-ensure-safely (name _keyword ensure rest state)
  (let* ((body (use-package-process-keywords name rest state))
         (ensure (and (not (plist-member rest :vc)) ensure)))
    ;; We want to avoid installing packages when the `use-package' macro is
    ;; being macro-expanded by elisp completion (see `lisp--local-variables'),
    ;; but still install packages when byte-compiling, to avoid requiring
    ;; `package' at runtime.
    (if (bound-and-true-p byte-compile-current-file)
        ;; Eval when byte-compiling,
        (progn
          (funcall use-package-ensure-function name ensure state)
          body)
      ;;  or else wait until runtime.
      (list `(when (or (,use-package-ensure-function ',name ',ensure ',state)
                       (package-installed-p ',name))
               ,@body)))))

(provide 'jacob-use-package)

;;; jacob-use-package.el ends here
