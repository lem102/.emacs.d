;;; init.el --- Jacob's main init file. -*-lexical-binding: t-*-
;;; Commentary:
;;; Code:

(global-unset-key (kbd "C-z"))

(defmacro jacob-is-installed (package &rest body)
  "If PACKAGE is installed, evaluate BODY.
Used when attempting to lazy load PACKAGE."
  (declare (indent 1))
  `(when (package-installed-p ,package)
     ,@body))

(defmacro jacob-try-require (feature &rest body)
  "Attempt to require FEATURE.
If successful, evaluate BODY.
Used to eagerly load FEATURE."
  (declare (indent 1))
  `(when (require ,feature nil 'noerror)
     ,@body))

(setq package-archives '(
                         ("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")
                         ))

(setq package-selected-packages '(
                                  ahk-mode
                                  csharp-mode
                                  web-mode
                                  go-mode
                                  sml-mode
                                  json-mode
                                  csv-mode
                                  powershell
                                  yaml-mode
                                  selectrum
                                  consult
                                  orderless
                                  selectrum-prescient
                                  marginalia
                                  edit-server
                                  goto-last-change
                                  eglot
                                  auctex
                                  expand-region
                                  projectile
                                  restart-emacs
                                  color-theme-sanityinc-tomorrow
                                  which-key
                                  xah-fly-keys
                                  ))

(package-install-selected-packages)
(package-autoremove)

(let ((jacob-config-directory (concat user-emacs-directory "jacob-init/")))
  (dolist (config-file (directory-files-recursively jacob-config-directory "\\.el$"))
    (load config-file)))

(load (concat user-emacs-directory "jacob-bindings.el"))

(provide 'init)
;;; init.el ends here
