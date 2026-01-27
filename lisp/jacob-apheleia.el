;;; jacob-apheleia.el --- Configuration for apheleia

;;; Commentary:
;; 

;;; Code:

(defun jacob-apheleia-config ()
  "Apply configuration for `apheleia'."
  (require 'jacob-apheleia-functions)
  (add-to-list 'apheleia-formatters '(csharpier "dotnet" "csharpier" "--write-stdout"))
  (add-to-list 'apheleia-mode-alist '(csharp-ts-mode . csharpier))

  (add-to-list 'apheleia-formatters '(scalafmt "scalafmt" "--stdin" "--non-interactive" "--quiet" "--stdout"))

  (add-to-list 'apheleia-mode-alist '(scala-ts-mode . scalafmt))

  (add-to-list 'apheleia-formatters '(gdscript-formatter "gdscript-formatter"))

  (setf (alist-get 'gdscript-mode apheleia-mode-alist)
        'gdscript-formatter)

  (setf (alist-get 'gdscript-ts-mode apheleia-mode-alist)
        'gdscript-formatter)

  (add-to-list 'apheleia-skip-functions #'jacob-apheleia-skip-function))

(use-package apheleia
  :blackout " ⚘"
  :hook (emacs-lisp-mode-hook scala-ts-mode-hook)
  :config (jacob-apheleia-config))

(provide 'jacob-apheleia)

;;; jacob-apheleia.el ends here
