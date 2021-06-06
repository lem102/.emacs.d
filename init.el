;;   (defun jacob-load (filename)
;;     "Loads FILENAME from the jacob-init directory in .emacs.d.
;; The name will never be changed ;)"
;;     (load (concat user-emacs-directory "jacob-init/" filename) nil 'nomessage))

;; (jacob-load "jacob-init-main.el")

(defun jacob-ensure-package-installed (package)
  "Ensures that PACKAGE is installed."
  (unless (or (package-installed-p package)
              (require package nil 'noerror))
    (unless (assoc package package-archive-contents)
      (package-refresh-contents))
    (package-install package)))

(defun jacob-demand-external-package (package)
  (jacob-ensure-package-installed package)
  (require package))

(setq package-selected-packages nil)
(setq package-archives '(
                         ("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")
                         ))

(let ((jacob-config-directory (concat user-emacs-directory "jacob-init/")))
  (dolist (config-file (directory-files-recursively jacob-config-directory "\\.el$"))
          (load config-file)))


