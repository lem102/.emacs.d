;;; jacob-eshell.el --- Eshell configuration -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(defun jacob-eshell-windows-confirm-terminate-batch-job ()
  "Type y and enter to terminate batch job after sending ^C."
  (when (not (null eshell-process-list))
    (insert "y")
    (eshell-send-input)))

(provide 'jacob-eshell)

;;; jacob-eshell.el ends here
