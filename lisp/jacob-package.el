;;; jacob-package.el --- Utilities for package.el  -*- lexical-binding: t; -*-

;;; Commentary:
;; 

;;; Code:

;; TODO: review this, AI generated code
(defun jacob-package-upgrade-only-builtins ()
  "Upgrade all built-in packages to their latest versions."
  (interactive)
  (package-refresh-contents)
  (dolist (pkg-name package-activated-list)
    (when (and (package-built-in-p pkg-name)
               (jacob-package-upgradeable-p pkg-name))
      (message "Upgrading built-in: %s" pkg-name)
      (package-install pkg-name))))

(defun jacob-package-upgradeable-p (pkg-name)
  "Return non-nil if PKG-NAME has a newer version available in archives."
  (let* ((available-pkg (cadr (assq pkg-name package-archive-contents)))
         (installed-pkg (cadr (assq pkg-name package-alist))))
    (when (and available-pkg installed-pkg)
      (version-list-< (package-desc-version installed-pkg)
                      (package-desc-version available-pkg)))))

;; TODO: include upgrade script?

(provide 'jacob-package)

;;; jacob-package.el ends here
