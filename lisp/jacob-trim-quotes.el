;;; jacob-trim-quotes.el --- trim the quotes from strings when pasting them into another string  -*- lexical-binding: t; -*-

;;; Commentary:

;; This package provides a mode to prevent yanking a programming
;; language string from the `kill-ring' into another programming
;; language string in the current buffer.

;;; Code:

(defun jacob-trim-quotes--trim (text)
  "If TEXT is a string, remove the leading and ending quotes."
  (if (string-match "^\\([[:space:]]*\\)\"\\(.*\\)\"\\([[:space:]]*\\)$" text)
      (concat (match-string 1 text)
              (match-string 2 text)
              (match-string 3 text))
    text))

(defun jacob-trim-quotes--point-in-string-p ()
  "Return non-nil if point is inside a string."
  (nth 3 (syntax-ppss)))

(defun jacob-trim-quotes--yank-transform (text)
  "Trim quotes from TEXT if point is inside a string."
  (if (jacob-trim-quotes--point-in-string-p)
      (jacob-trim-quotes--trim text)
    text))

(define-minor-mode jacob-trim-quotes-mode
  "Minor mode to prevent the yanking of strings from messing up existing strings at point."
  nil
  (if jacob-trim-quotes-mode
      (add-hook 'yank-transform-functions #'jacob-trim-quotes--yank-transform nil "LOCAL")
    (remove-hook 'yank-transform-functions #'jacob-trim-quotes--yank-transform "LOCAL")))

(provide 'jacob-trim-quotes)

;;; jacob-trim-quotes.el ends here
