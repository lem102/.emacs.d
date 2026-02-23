;;; jacob-consult.el --- Functions for `consult'  -*- lexical-binding: t; -*-


;;; Commentary:
;; 

;;; Code:

;;;###autoload
(defun jacob-consult-buffer-state-no-tramp ()
  "Buffer state function that doesn't preview Tramp buffers."
  (let ((orig-state (consult--buffer-state))
        (filter (lambda (action candidate)
                  (if (and candidate
                           (or (eq action 'return)
                               (let ((buffer (get-buffer candidate)))
                                 (and buffer
                                      (not (file-remote-p (buffer-local-value 'default-directory buffer)))))))
                      candidate
                    nil))))
    (lambda (action candidate)
      (funcall orig-state action (funcall filter action candidate)))))

;;;###autoload
(defun jacob-project-search ()
  "Wrapper for grep commands."
  ;; FIXME: should cd to the project directory first
  ;; TODO: do i need this?
  (interactive)
  (if (vc-find-root default-directory ".git")
      (consult-git-grep)
    (consult-grep)))

(provide 'jacob-consult)

;;; jacob-consult.el ends here

