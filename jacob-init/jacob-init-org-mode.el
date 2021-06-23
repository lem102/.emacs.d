(with-eval-after-load 'org-mode
  (defun jacob-org-babel-tangle-delete-newline ()
    "Some code to get rid of the newline org babel likes to add
in when it tangles into a file."
    (goto-char (point-max))
    (delete-trailing-whitespace)
    (backward-delete-char 1)
    (save-buffer))

  (add-hook 'org-babel-post-tangle-hook 'jacob-org-babel-tangle-delete-newline)

  (setq org-latex-pdf-process (list "latexmk -pdf %f -shell-escape")) ; probably requires texlive

  ;; for syntax highlighting in latex export. requires the minted
  ;; latex package, and pygmentize, a python package.
  (setq org-latex-listings 'minted)
  (add-to-list 'org-latex-packages-alist '("newfloat" "minted"))

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((octave . t))))
