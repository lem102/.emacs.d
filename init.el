;; plans for the configuration

;; i think i need to figure out what is the simplest way of managing my packages, and setting them up so that emacs starts fast. is this use package, or leaf? is there some other technique that i can use? You need to stop being edgy about this, and start learning.

;; so the choice is, simplicity or (startup) speed? Food for thought. now go to bed

(defun jacob-load (filename)
  "Loads FILENAME from the jacob-init directory in .emacs.d. The name will never be changed ;)"
  (load (concat user-emacs-directory "jacob-init/" filename) nil 'nomessage))

(jacob-load "jacob-init-start.el")
