;;; jacob-bookmark.el --- Support library for `bookmark'.  -*- lexical-binding: t; -*-

;;; Commentary:
;; 

;;; Code:


;;;###autoload
(defun jacob-bookmark-command (bookmark)
  "Launch the command stored in BOOKMARK.

Intended for running applications."
  (let ((command (bookmark-get-filename bookmark)))
    (start-process-shell-command command nil command)))

;;;###autoload
(defun jacob-bookmark-firefox (bookmark)
  "Open BOOKMARK in firefox."
  (let ((command (concat "firefox-esr "
                         (bookmark-get-filename bookmark))))
    (start-process-shell-command command
                                 nil
                                 command)))

;;;###autoload
(defun jacob-bookmark-chrome (bookmark)
  "Open BOOKMARK in chrome."
  (let ((command (concat "google-chrome "
                         (bookmark-get-filename bookmark))))
    (start-process-shell-command command
                                 nil
                                 command)))

(provide 'jacob-bookmark)

;;; jacob-bookmark.el ends here
