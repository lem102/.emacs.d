;;; jacob-llm.el --- LLM configurations for Jacob. -*-lexical-binding: t; -*-

;;; Commentary:
;;; This file contains configurations for Large Language Model integrations.

;;; Code:

(use-package gptel
  :defer t
  :custom
  (gptel-default-mode #'org-mode)
  (gptel-confirm-tool-calls t))

(use-package mcp
  :after gptel)

(use-package elisp-dev-mcp
  :after gptel)

(provide 'jacob-llm)

;;; jacob-llm.el ends here
