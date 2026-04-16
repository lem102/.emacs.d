;;; jacob-format-words.el --- Utilities for formatting words  -*- lexical-binding: t; -*-

;;; Commentary:
;; 

;;; Code:

(defvar jacob-format-words-style-and-start nil
  "Pair of currently selected style and starting point.
If nil, means you havent used the command for the first time yet.")

(defun jacob-format-words ()
  "Command for formating words into identifiers when writing code.

On first use, ask for formatting style (e.g. kebab, camel,
etc).  Format one word backwards in selected style and store the style
and the position of point after formatting, return point to where it
was when command called.

On consecutive use, apply stored formatting to word before stored
point."
  (interactive)

  (undo-boundary)

  (unless (eq last-command this-command)
    (setq jacob-format-words-style-and-start (cons (pcase major-mode
                                                     ('emacs-lisp-mode ?k)
                                                     (_ (read-char-from-minibuffer "select style: " '(?c ?p ?k ?s ?S))))
                                                   (point))))

  (save-excursion
    (let* ((style (car jacob-format-words-style-and-start))
           (format-position (cdr jacob-format-words-style-and-start)))
      (goto-char format-position)
      (pcase style
        (?c (progn
              (backward-word)
              (capitalize-word 1)
              (backward-word)
              (delete-char -1)))
        (?p (progn
              (backward-word)
              (capitalize-word 1)
              (backward-word)
              (delete-char -1)
              (backward-word)
              (capitalize-word 1)))
        (?k (progn
              (backward-word)
              (delete-char -1)
              (insert-char ?-)
              (backward-char)))
        (?s (progn
              (backward-word)
              (delete-char -1)
              (insert-char ?_)
              (backward-char)))
        (?S (progn
              (backward-word)
              (upcase-word 1)
              (backward-word)
              (delete-char -1)
              (insert-char ?_)
              (backward-char)
              (backward-word)
              (upcase-word 1))))

      (setq jacob-format-words-style-and-start (cons style (point))))))

(provide 'jacob-format-words)

;;; jacob-format-words.el ends here
