;;; jacob-require.el --- easy install/loading of packages  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defvar jacob-require-already-refreshed nil
  "If nil, haven't refreshed packages with `jacob-require' yet.")

(defun jacob-require-ensure-installed (package &optional vc)
  "Ensure PACKAGE is installed.

If VC is provided, it is passed to `package-vc-install' to
install the package rather than using `package-install'."
  (unless (package-installed-p package)
    (if vc
        (package-vc-install vc)
      (unless jacob-require-already-refreshed
        (package-refresh-contents)
        (setq jacob-require-already-refreshed t))
      (package-install package))))

(defmacro jacob-require (package &optional vc)
  "Ensure the PACKAGE is installed, then `require' it.

VC is a url pointing to a git repository."
  `(progn
     (jacob-require-ensure-installed ,package ,vc)
     (require ,package)))

(provide 'jacob-require)

;;; jacob-require.el ends here
