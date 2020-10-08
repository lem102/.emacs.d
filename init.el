(defmacro measure-time (&rest body)
  `(let ((time (current-time)))
     ,@body
     (message "%.06f" (float-time (time-since time)))))

(require 'package)

(setq package-enable-at-startup nil)
(add-to-list 'package-archives
			 '("melpa" . "https://melpa.org/packages/"))
(unless package--initialized (package-initialize))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; go to the nice org file :)
(measure-time (require 'org))
(measure-time (org-babel-load-file (expand-file-name "~/.emacs.d/config.org")))
