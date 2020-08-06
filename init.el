(setq gc-cons-threshold (* 50 1000 1000))

(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
			 '("melpa" . "https://melpa.org/packages/"))

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;;;;

;; go to the nice org file :)
(org-babel-load-file (expand-file-name "~/.emacs.d/config.org"))

;;;;


(put 'dired-find-alternate-file 'disabled nil)

(setq gc-cons-threshold (* 2 1000 1000))
(put 'narrow-to-page 'disabled nil)
