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
                                         ("\\.mp4\\'" "mpv")))

  (with-eval-after-load "ryo-modal"
    (ryo-modal-major-mode-keys 'dired-mode
                               ("s" dired-find-file)
                               ("d" dired-do-delete)
                               ("q" quit-window)
                               ("i" dired-previous-line)
                               ("k" dired-next-line)
                               ("e" dired-mark)
                               ("r" dired-unmark)
                               ("g" revert-buffer)
                               ("x" dired-do-rename)
                               ("c" dired-do-copy)
                               ("u" dired-up-directory)
                               ("j" dired-goto-file))))

(use-package wdired-mode
  :defer t
  :config
  (with-eval-after-load "ryo-modal"
    (add-hook 'wdired-mode-hook #'global-ryo-modal-mode-refresh-keys)
    (advice-add #'wdired-change-to-dired-mode
                :after
                #'global-ryo-modal-mode-refresh-keys)))

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
