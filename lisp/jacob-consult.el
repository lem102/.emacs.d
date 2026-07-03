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
(defvar jacob-consult-source-compile
  `( :name     "Compile"
     :narrow   ?c
     :category buffer
     :face     consult-buffer
     :history  buffer-name-history
     :state    ,#'consult--buffer-state
     :default  t
     :items
     ,(lambda () (consult--buffer-query :sort 'visibility
                                        :as #'consult--buffer-pair
                                        :mode #'compilation-mode)))
  "Compilation buffer source for `consult-buffer'.")

;;;###autoload
(defvar jacob-consult-source-magit
  `( :name     "Magit"
     :narrow   ?g
     :category buffer
     :face     consult-buffer
     :history  buffer-name-history
     :state    ,#'consult--buffer-state
     :default  t
     :items
     ,(lambda () (consult--buffer-query :sort 'visibility
                                        :as #'consult--buffer-pair
                                        :mode #'magit-status-mode)))
  "Magit buffer source for `consult-buffer'.")

(provide 'jacob-consult)

;;; jacob-consult.el ends here

