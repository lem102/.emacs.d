(defun jacob-load (filename)
  (load (concat user-emacs-directory "jacob-init/" filename) nil 'nomessage))

(jacob-load "jacob-init-start.el")
