;;; jacob-package.el --- Utilities for package.el  -*- lexical-binding: t; -*-

;;; Commentary:
;; 

;;; Code:

;;;###autoload
(defun jacob-package-upgrade-all ()
  "Upgrade all packages in `package-selected-packages'."
  (interactive)
  (package-refresh-contents)
  (dolist (p (cl-intersection package-selected-packages (package--upgradeable-packages)))
    (package-upgrade p))
  (package-autoremove))

(provide 'jacob-package)

;;; jacob-package.el ends here
