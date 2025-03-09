;;; jacob-linux.el --- Linux module -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'jacob-require)

(jacob-require 'eat)
(add-hook 'eshell-mode-hook #'eat-eshell-mode)

(jacob-require 'pdf-tools)

(jacob-require 'pulseaudio-control)

;; use `pulseaudio-control-select-sink-by-name' to set the "sink" (the
;; output device)

(jacob-require 'bluetooth)

;; use `bluetooth-list-devices' to display the bluetooth buffer

(provide 'jacob-linux)

;;; jacob-linux.el ends here
