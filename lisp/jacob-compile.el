;;; jacob-compile.el --- helpers for compile.el  -*- lexical-binding: t; -*-

;;; Commentary:
;; 

;;; Code:

(defun jacob-compilation-project-file ()
  "Somehow determine the filepath for the compilation error"
  (save-match-data
    (let ((filename-from-error (match-string 1)) ;; FIXME: this will break when filename is not in first re group
          )
      (seq-find (lambda (f)
                  (string= (file-name-nondirectory f)
                           filename-from-error))
                (project-files (project-current))))))

(provide 'jacob-compile)

;;; jacob-compile.el ends here
