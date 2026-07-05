;;; jacob-alternatives.el --- Utilities for configuring alternative packages to be used.  -*- lexical-binding: t; -*-

;;; Commentary:
;; 

;;; Code:

(defun jacob-weight-configure-features (_symbol weight)
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
  :set #'jacob-weight-configure-features)

(provide 'jacob-alternatives)

;;; jacob-alternatives.el ends here
