;;; jacob-wgrep.el --- Utilities for wgrep  -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:

;;;###autoload
(defun jacob-wgrep-handle-w ()
  "Handle the \"w\" key being pressed in grep-mode."
  (interactive)
  (if (and (boundp wgrep-prepared) wgrep-prepared)
      (jacob-shrink-whitespaces)
    (wgrep-change-to-wgrep-mode)))

;;;###autoload
(defun jacob-wgrep-handle-e ()
  "Handle the \"e\" key being pressed in grep-mode."
  (interactive)
  (if (and (boundp wgrep-prepared) wgrep-prepared)
      (puni-backward-kill-word)
    (previous-error-no-select)))

;;;###autoload
(defun jacob-wgrep-handle-r ()
  "Handle the \"r\" key being pressed in grep-mode."
  (interactive)
  (if (and (boundp wgrep-prepared) wgrep-prepared)
      (puni-forward-kill-word)
    (next-error-no-select)))

(provide 'jacob-wgrep)

;;; jacob-wgrep.el ends here
