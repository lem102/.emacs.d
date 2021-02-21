(use-package org
  :mode ("\\.org\\'" . org-mode)
  :config
  
  (defun jacob-org-babel-tangle-delete-newline ()
    "Some code to get rid of the newline org babel likes 
to add in when it tangles into a file."
    (goto-char (point-max))
    (delete-trailing-whitespace)
    (backward-delete-char 1)
    (save-buffer))

  (add-hook 'org-babel-post-tangle-hook 'jacob-org-babel-tangle-delete-newline)

  (defun jacob-config-regenerate-yasnippets ()
    "Delete snippet files, then tangle from snippet central.
Afterwards, reload snippets."
    (interactive)
    (let* ((snippet-directory-path (expand-file-name "~/.emacs.d/snippets/"))
           (snippet-central-path (concat snippet-directory-path "snippet-central.org"))
           (snippet-dirs (mapcar (lambda (snippet-major-mode-directory)
                                   (concat snippet-directory-path snippet-major-mode-directory))
                                 '("cc-mode" "java-mode" "csharp-mode"))))
      (mapc (lambda (snippet-dir)
              (mapc 'delete-file (directory-files snippet-dir t directory-files-no-dot-files-regexp))) snippet-dirs)
      (org-babel-tangle-file snippet-central-path)
      (yas-reload-all)))

  (setq-default yas-indent-line 'auto)  ; used to be set to 'fixed, not sure why. auto allows for us to not worry about indenting when we are doing else if.

  (setq org-latex-pdf-process (list "latexmk -pdf %f")) ; probably requires texlive
  )
