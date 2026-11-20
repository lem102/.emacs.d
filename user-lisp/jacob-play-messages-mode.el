;;; jacob-play-messages-mode.el --- Major mode for play framework messages files  -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:

(defvar jacob-play-messages-font-lock-keywords
  '(("^[[:alpha:].]+" . font-lock-variable-name-face)
    ("^#.*" . font-lock-comment-face))
  "Syntax highlighting for Play Framework message files.")

(define-derived-mode jacob-play-messages-mode prog-mode "Play Messages"
  "Major mode for Play Framework message files."
  (setq-local comment-start "#")
  (setq-local font-lock-defaults '(jacob-play-messages-font-lock-keywords
                                   "KEYWORDS-ONLY")))

(add-to-list 'auto-mode-alist '("`\\(messages.en\\|messages.cy\\)'" . jacob-play-messages-mode))

(provide 'jacob-play-messages-mode)

;;; jacob-play-messages-mode.el ends here
