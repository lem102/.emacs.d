;;; jacob-dired.el --- Configuration for dired. -*- lexical-binding: t; -*-

;;; Commentary:
;; 

;;; Code:

(use-package ls-lisp
  :defer t
  :init
  (setq ls-lisp-use-insert-directory-program nil
        ls-lisp-dirs-first t))

(use-package dired
  :defer t
  :init
  (add-hook 'dired-mode-hook #'dired-hide-details-mode)
  (add-hook 'dired-mode-hook #'auto-revert-mode)
  :config
  (setopt dired-recursive-copies 'always
          dired-dwim-target t
          dired-listing-switches "-hal" ; the h option needs to come first 🙃
          dired-guess-shell-alist-user '(("\\.mkv\\'" "mpv")
                                         ("\\.mp4\\'" "mpv"))))

(use-package dired-aux
  :defer t
  :config
  (setopt dired-vc-rename-file t))

(use-package dired-rsync
  :defer t
  :after dired
  :config
  (add-to-list 'mode-line-misc-info '(:eval dired-rsync-modeline-status 'append)))

(provide 'jacob-dired)

;;; jacob-dired.el ends here
