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

;; Exercism

;; TODO: investigate `project-vc-extra-root-markers' as an alternative to the below

;;;###autoload
(defun jacob-project-try-exercism (dir)
  "Find exercism project in DIR."
  (when-let ((dir (locate-dominating-file dir ".exercism")))
    (list 'exercism dir)))

;;;###autoload
(cl-defmethod project-root ((project (head exercism)))
  "Get the PROJECT root for an Exercism project."
  (nth 1 project))

(provide 'jacob-project)

;;; jacob-project.el ends here
