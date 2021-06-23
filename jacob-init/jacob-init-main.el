;;; -*- lexical-binding: t -*-
;;;
;;; Code:

(jacob-try-require 'edit-server
  (edit-server-start))

(jacob-is-installed 'ahk-mode
  (push '("\\.ahk\\$" . ahk-mode) auto-mode-alist))

(jacob-is-installed 'goto-last-change
  (global-set-key (kbd "C-z j") 'goto-last-change)
  (global-set-key (kbd "C-z l") 'goto-last-change-reverse))
