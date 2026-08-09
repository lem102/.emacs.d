;;; jacob-project.el --- utilities for project.el  -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:

(require 'project)

;;;###autoload
(defun jacob-dired-in-other-project ()
  "Switch to another project and open `dired' there."
  (interactive)
  (dired (project-prompt-project-dir)))

;;;###autoload
(defun jacob-project-visit-test ()
  "If a test file that corresponds to the current buffer exists, visit it.

If a non-test file that corresponds to the current buffer exists, visit it.

Currently this command assumes we are in a scala project."
  (interactive)
  (let* ((filename (file-name-nondirectory (buffer-file-name)))
         (extension (file-name-extension filename))
         (is-test-file (string-match-p "Spec$" (file-name-sans-extension filename)))
         (test-name (if is-test-file
                        filename
                      (format "%s.%s"
                              (concat (file-name-sans-extension filename) "Spec")
                              extension)))
         (implementation-name (if is-test-file
                                  (with-temp-buffer
                                    (insert filename)
                                    (re-search-backward "^\\(.+\\)Spec\..+$" nil "NOERROR")
                                    (format "%s.%s" (match-string 1) extension))
                                filename)))
    (find-file (seq-find (lambda (f)
                           (string= (file-name-nondirectory f)
                                    (if is-test-file
                                        implementation-name
                                      test-name)))
                         (project-files (project-current))))))

(provide 'jacob-project)

;;; jacob-project.el ends here
