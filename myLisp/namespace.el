(defun namespace-get ()
  (interactive)
  (substring
   (s-replace "/"
              "."
              (file-relative-name (file-name-sans-extension (file-name-directory (buffer-file-name)))
                                  (file-name-directory (directory-file-name (projectile-project-root)))))
   0
   -1))
