;;; jacob-wgrep.el --- Utilities for wgrep  -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:

;;;###autoload
(defun jacob-wgrep-handle-w ()
  "Handle the \"w\" key being pressed in grep-mode."
  (interactive)
  (if (and (boundp wgrep-prepared) wgrep-prepared)
      (funcall (keymap-lookup jacob-modal-editing-command-mode-map "w"))
    (wgrep-change-to-wgrep-mode)))

;;;###autoload
(defun jacob-wgrep-handle-e ()
  "Handle the \"e\" key being pressed in grep-mode."
  (interactive)
  (if (and (boundp wgrep-prepared) wgrep-prepared)
      (funcall (keymap-lookup jacob-modal-editing-command-mode-map "e"))
    (previous-error-no-select)))

;;;###autoload
(defun jacob-wgrep-handle-r ()
  "Handle the \"r\" key being pressed in grep-mode."
  (interactive)
  (if (and (boundp wgrep-prepared) wgrep-prepared)
      (funcall (keymap-lookup jacob-modal-editing-command-mode-map "r"))
    (next-error-no-select)))

(provide 'jacob-wgrep)

;;; jacob-wgrep.el ends here
