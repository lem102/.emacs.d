;;; jacob-cecli.el --- integration with cecli

;;; Commentary:
;; 

;;; Code:

(defvar jacob-cecli-arguments '("--linear-output" "--watch-files")
  "Arguments to pass to the cecli process.")

(defun jacob-cecli ()
  "Start or switch to the cecli buffer for the current project."
  (interactive)
  (pop-to-buffer
   (let* ((project (project-current t))
          (buffer-name (format "%s-cecli" (project-name project))))
     (if-let ((buffer (get-buffer buffer-name)))
         buffer
       (let ((default-directory (project-root project)))
         (apply #'make-comint buffer-name "cecli" nil jacob-cecli-arguments))))))

(defun jacob-cecli-add-file ()
  "Add the current file to cecli."
  (interactive)
  (when-let* ((file-path (buffer-file-name (current-buffer)))
              (relative-path (file-relative-name file-path (project-root (project-current)))))
    (comint-send-string (jacob-cecli)
                        (format "/add %s\n" relative-path))))

(provide 'jacob-cecli)

;;; jacob-cecli.el ends here
