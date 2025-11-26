;;; jacob-consult-functions.el --- Functions for `jacob-consult'


;;; Commentary:
;; 

;;; Code:

;; FIXME: This function is buggered currently.
;; The function works if you move the defun to the scratch buffer and
;; evaluate it there.
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

(defun jacob-project-search ()
  "Wrapper for grep commands."
  (interactive)
  (if (vc-find-root default-directory ".git")
      (consult-git-grep)
    (consult-grep)))

(provide 'jacob-consult-functions)

;;; jacob-consult-functions.el ends here

