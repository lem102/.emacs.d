;;; jacob-dired.el --- Configuration for dired. -*- lexical-binding: t; -*-

;;; Commentary:
;; 

;;; Code:

(use-package ls-lisp
  :demand jacob-is-server-running
  :defer t
  :init
  (setq ls-lisp-use-insert-directory-program nil
        ls-lisp-dirs-first t))

(use-package dired
  :demand jacob-is-server-running
  :defer t
  :init
  (add-hook 'dired-mode-hook #'dired-hide-details-mode)
  (add-hook 'dired-mode-hook #'auto-revert-mode)

  (with-eval-after-load "xah-fly-keys"
    (jacob-xfk-bind-for-mode dired-mode
                             "s" #'dired-find-file
                             "d" #'dired-do-delete ; we skip the "flag, delete" process as files are sent to system bin on deletion
                             "q" #'quit-window
                             "i" #'dired-previous-line
                             "k" #'dired-next-line
                             "e" #'dired-mark
                             "r" #'dired-unmark
                             "g" #'revert-buffer
                             "x" #'dired-do-rename
                             "c" #'dired-do-copy
                             "u" #'dired-up-directory
                             "j" #'dired-goto-file))
  :config
  (setopt dired-recursive-copies 'always
          dired-dwim-target t
          dired-listing-switches "-hal" ; the h option needs to come first 🙃
          dired-guess-shell-alist-user '(("\\.mkv\\'" "mpv")
                                         ("\\.mp4\\'" "mpv"))))

(use-package dired-aux
  :defer t
  :after dired
  :config
  (setopt dired-vc-rename-file t))

(use-package dired-rsync
  :defer t
  :after dired
  :config
  (add-to-list 'mode-line-misc-info '(:eval dired-rsync-modeline-status 'append)))

(provide 'jacob-dired)

;;; jacob-dired.el ends here
