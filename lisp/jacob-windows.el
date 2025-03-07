;;; jacob-windows.el --- Windows configuration  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'grep)
;; TODO: find better solution
(setopt find-program "C:/Program Files (x86)/GnuWin32/bin/find.exe")

(require 'esh-mode)
(require 'esh-proc)

(defun jacob-confirm-terminate-batch-job ()
  "Type y and enter to terminate batch job after sending ^C."
  (when (not (null eshell-process-list))
    (insert "y")
    (eshell-send-input)))

(advice-add 'eshell-interrupt-process :after #'jacob-confirm-terminate-batch-job)

(provide 'jacob-windows)

;;; jacob-windows.el ends here
