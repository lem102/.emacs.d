(use-package org
  :mode ("\\.org\\'" . org-mode)
  :config
  ;; some code to get rid of the newline org babel likes to add in when it tangles into a file
  (defun jacob-org-babel-tangle-delete-newline ()
    (goto-char (point-max))
    (delete-trailing-whitespace)
    (backward-delete-char 1)
    (save-buffer))

  (add-hook 'org-babel-post-tangle-hook 'jacob-org-babel-tangle-delete-newline)

  (setq-default yas-indent-line 'auto)  ; used to be set to 'fixed, not sure why. auto allows for us to not worry about indenting when we are doing else if.
  ;; (add-to-list 'org-structure-template-alist
  ;;              '("el" "#+BEGIN_SRC emacs-lisp\n?\n#+END_SRC"))

  (setq org-latex-pdf-process (list "latexmk -pdf %f")) ; probably requires texlive

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((octave . t))))
