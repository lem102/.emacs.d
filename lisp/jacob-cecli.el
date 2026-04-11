;;; jacob-cecli.el --- integration with cecli

;;; Commentary:
;; 

;;; Code:

(define-derived-mode jacob-cecli-mode comint-mode "CECLI"
  "Major mode for interacting with cecli."
  (setq-local comint-prompt-regexp "^\\(ask\\)?> *"))

(defun jacob-cecli ()
  "Start or switch to the cecli comint buffer."
  (interactive)
  (let ((buffer-name "*cecli*"))
    (unless (comint-check-proc buffer-name)
      (let ((process (start-process "cecli" buffer-name "cecli" "--model" "gemini/gemini-3.1-flash-lite-preview" "--linear-output")))
        (with-current-buffer buffer-name
          (jacob-cecli-mode))))
    (pop-to-buffer buffer-name)))

(provide 'jacob-cecli)

;;; jacob-cecli.el ends here
