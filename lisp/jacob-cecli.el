;;; jacob-cecli.el --- integration with cecli

;;; Commentary:
;; 

;;; Code:

(defun jacob-cecli ()
  "Start or switch to the cecli buffer for the current project."
  (interactive)
  (pop-to-buffer
   (let* ((proj (project-current t))    ; rename proj to project AI!
          (buffer-name (format "%s-cecli" (project-name proj))))
     (if-let* ((buffer (get-buffer buffer-name)))
         buffer
       (let ((default-directory (project-root proj)))
         (make-comint buffer-name
                      "cecli"
                      nil
                      "--linear-output"
                      "--watch-files"))))))

(defun jacob-cecli-add-file ()
  "Add the current file to cecli."
  (interactive)
  (when-let* ((file-path (buffer-file-name (current-buffer)))
              (relative-path (file-relative-name file-path (project-root (project-current)))))
    (comint-send-string (jacob-cecli)
                        (format "/add %s\n" relative-path))))

(provide 'jacob-cecli)

;;; jacob-cecli.el ends here
