;;; jacob-device.el --- Control the features that are configured for different devices. -*- lexical-binding: t; -*-

;;; Commentary:
;; 

;;; Code:

(defun jacob-device--handle-weight-change (_symbol weight)
  "Configure features suitable for WEIGHT."
  (cond ((eq weight 'heavy)
         (setq consult-preview-key 'any)
         (vertico-mode 1)
         (which-key-mode 1)
         (keymap-set jacob-modal-editing-keymap "SPC p v" #'magit-project-status))
        (t
         (setq consult-preview-key nil)
         (when (featurep 'vertico)
           (vertico-mode 0)
           (unload-feature 'vertico))
         (when (featurep 'which-key)
           (which-key-mode 0)
           (unload-feature 'which-key))
         (keymap-set jacob-modal-editing-keymap "SPC p v" #'project-vc-dir))))

;;;###autoload
(defcustom jacob-device-weight 'light
  "The \"weight\" of the current device.

Can be either lightweight or heavyweight. This will affect features
enabled or disabled in the init files. By default, the device is
considered lightweight."
  :type '(radio (const light)
                (const heavy))
  :options '(light heavy)
  :set #'jacob-device--handle-weight-change)

(provide 'jacob-device)

;;; jacob-device.el ends here
