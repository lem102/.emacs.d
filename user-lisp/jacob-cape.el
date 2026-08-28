;;; jacob-cape.el --- Utilities for `cape.el'  -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:

(require 'cape)
(require 'elisp-mode)

;;;###autoload
(defun jacob-cape-configure-elisp-mode ()
  "Configure capfs for elisp-mode using cape."
  (setq-local completion-at-point-functions (list (cape-capf-super #'elisp-completion-at-point
                                                                   :with #'cape-dabbrev))))

(provide 'jacob-cape)

;;; jacob-cape.el ends here
