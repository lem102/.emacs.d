(defun jacob-load (filename)
  "Loads FILENAME from the jacob-init directory in .emacs.d. The name will never be changed ;)"
  (load (concat user-emacs-directory "jacob-init/" filename) nil 'nomessage))

(jacob-load "jacob-init-start.el")
