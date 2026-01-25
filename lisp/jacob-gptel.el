;;; jacob-gptel.el --- gptel configuration -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(use-package gptel
  :defer t
  :custom
  (gptel-backend (gptel-make-ollama "Ollama"
                   :host "localhost:11434"
                   :stream t
                   :models '(qwen2.5-coder:7b)))
  (gptel-default-mode #'org-mode)
  (gptel-confirm-tool-calls t))

(provide 'jacob-gptel)

;;; jacob-gptel.el ends here
