;;; -*- lexical-binding: t -*-

(use-package edit-server
  :ensure t
  :config
  (edit-server-start))

(use-package ahk-mode
  :ensure t
  :mode ("\\.ahk\\$" . ahk-mode))

(use-package goto-last-change
  :bind
  ("C-z j" . goto-last-change)
  ("C-z l" . goto-last-change-reverse))
