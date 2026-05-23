;;; jacob-tea.el --- Utilities for brewing tea  -*- lexical-binding: t; -*-

;;; Commentary:
;; 

;;; Code:

(require 'tmr)

;;;###autoload
(defun jacob-tea-boil ()
  "Start a timer to boil the kettle."
  (interactive)
  (tmr "2m" "Kettle boiled"))

;;;###autoload
(defun jacob-tea-brew ()
  "Start a timer for brewing tea."
  (interactive)
  (tmr "3m" "Tea brewed"))

(provide 'jacob-tea)

;;; jacob-tea.el ends here
