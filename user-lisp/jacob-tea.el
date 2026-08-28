;;; jacob-tea.el --- Utilities for brewing tea  -*- lexical-binding: t; -*-

;;; Commentary:
;; 

;;; Code:

(require 'tmr)

(defconst jacob-tea--boiled-message "Kettle boiled")

(defconst jacob-tea--brewed-message "Tea brewed")

;; TODO: how to handle places where boiling is not necessary?
(defvar jacob-tea--state nil
  "Nil means no tea, or tea ready to drink.
`boiling' means the kettle is boiling.
`boiled' means the kettle has boiled.
`brewing' means the tea is brewing.
`brewed' means the tea has finished brewing.")

(defun jacob-tea--tmr-finished (tmr)
  "Function for `tmr-timer-finished-functions'.

TMR is a tmr."
  (when (string= jacob-tea--boiled-message (tmr--timer-description tmr))
    (setq jacob-tea--state 'boiled))
  (when (string= jacob-tea--brewed-message (tmr--timer-description tmr))
    (setq jacob-tea--state 'brewed)))

(defun jacob-tea--status ()
  "Return the status of the tea."
  (format "🫖~%s"
          (pcase jacob-tea--state
            ('boiling "boiling")
            ('boiled "boiled")
            ('brewing "brewing")
            ('brewed "brewed")
            (_ "ready"))))

(defvar jacob-tea--mode-line '(" " (:eval (jacob-tea--status))))
(put 'jacob-tea--mode-line 'risky-local-variable t)

(define-minor-mode jacob-tea-mode
  "Mode for brewing tea."
  :global t
  :lighter jacob-tea--mode-line
  (if jacob-tea-mode
      (add-to-list 'tmr-timer-finished-functions #'jacob-tea--tmr-finished)
    (setq tmr-timer-finished-functions
          (remove #'jacob-tea--tmr-finished
                  tmr-timer-finished-functions))))

;;;###autoload
(defun jacob-tea-boil ()
  "Start a timer to boil the kettle."
  (interactive)
  (jacob-tea-mode 1)
  (setq jacob-tea--state 'boiling)
  (tmr "2m" "Kettle boiled"))

;;;###autoload
(defun jacob-tea-brew ()
  "Start a timer for brewing tea."
  (interactive)
  (jacob-tea-mode 1)
  (setq jacob-tea--state 'brewing)
  (tmr "3m" "Tea brewed"))

(provide 'jacob-tea)

;;; jacob-tea.el ends here
