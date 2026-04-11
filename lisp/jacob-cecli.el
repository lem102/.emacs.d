;;; jacob-cecli.el --- integration with cecli

;;; Commentary:
;; 

;;; Code:

(defun jacob-cecli ()
  "Start cecli."
  (interactive)
  (comint-run "cecli" '("--model" "gemini/gemini-3.1-flash-lite-preview" "--linear-output"))
  (font-lock-add-keywords nil smerge-font-lock-keywords 'append))

(provide 'jacob-cecli)

;;; jacob-cecli.el ends here
