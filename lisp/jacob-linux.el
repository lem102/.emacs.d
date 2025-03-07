;;; jacob-linux.el --- Linux module -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(jacob-require 'pulseaudio-control)

;; use `pulseaudio-control-select-sink-by-name' to set the "sink" (the
;; output device)

(jacob-require 'bluetooth)

;; use `bluetooth-list-devices' to display the bluetooth buffer

(provide 'jacob-linux)

;;; jacob-linux.el ends here
