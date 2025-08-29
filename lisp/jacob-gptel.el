;;; jacob-gptel.el --- gptel configuration -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(use-package gptel
  :demand jacob-is-server-running
  :defer t
  :custom
  (gptel-default-mode #'org-mode)
  (gptel-confirm-tool-calls t))

(provide 'jacob-gptel)

;;; jacob-gptel.el ends here
