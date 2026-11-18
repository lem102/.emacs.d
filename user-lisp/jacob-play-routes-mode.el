;;; jacob-play-routes-mode.el --- Major mode for play framework routes file  -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:

(require 'xref)

(defvar jacob-play-routes-font-lock-keywords
  '(("^\\(GET\\|POST\\|PUT\\|DELETE\\|PATCH\\|OPTIONS\\|HEAD\\)" . font-lock-keyword-face)
    ("^#.*" . font-lock-comment-face)
    ("/[^ ]+" . font-lock-string-face))
  "Syntax highlighting for Play routes.")

(define-derived-mode jacob-play-routes-mode prog-mode "PlayRoutes"
  "Major mode for Play Framework routes files."
  (setq-local comment-start "#")
  (setq-local font-lock-defaults '(jacob-play-routes-font-lock-keywords
                                   "KEYWORDS-ONLY"))
  (add-hook 'xref-backend-functions #'jacob-play-routes-xref-backend nil t))

(add-to-list 'auto-mode-alist '("routes\\'" . jacob-play-routes-mode))

(defun jacob-play-routes-xref-backend ()
  "Xref backend for jacob-play-routes-mode."
  'jacob-play-routes)

(cl-defmethod xref-backend-definitions ((_backend (eql jacob-play-routes)) _symbol)
  ;; TODO: stretch goal, go to the method definition
  (let* ((case-fold-search nil))
    (save-excursion
      (beginning-of-line)
      (re-search-forward "[a-zA-Z]+Controller")
      (list (xref-make "x"
                       (xref-make-file-location (seq-find (lambda (f)
                                                            (string= (file-name-sans-extension (file-name-nondirectory f))
                                                                     (thing-at-point 'symbol "NO-PROPERTIES")))
                                                          (project-files (project-current)))
                                                1
                                                0))))))

(provide 'jacob-play-routes-mode)

;;; jacob-play-routes-mode.el ends here
