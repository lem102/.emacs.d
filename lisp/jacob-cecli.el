;;; jacob-cecli.el --- integration with cecli

;;; Commentary:
;; 

;;; Code:

(defun jacob-cecli ()
  "Start cecli."
  (interactive)
  (comint-run "cecli" '("--model" "gemini/gemini-3.1-flash-lite-preview" "--linear-output"))
  (font-lock-add-keywords nil smerge-font-lock-keywords 'append))

(defun jacob-cecli-add-file ()
  "Add the current file to cecli, relative to the git root."
  (interactive)
  (when-let* ((file-path (buffer-file-name (current-buffer)))
              (cecli-buffer (get-buffer "*cecli*"))
              (relative-path (file-relative-name file-path (vc-root-dir))))
    (comint-send-string cecli-buffer
                        (concat "/add " relative-path "\n"))))

;; TODO: support a cecli buffer per project

(provide 'jacob-cecli)

;;; jacob-cecli.el ends here
