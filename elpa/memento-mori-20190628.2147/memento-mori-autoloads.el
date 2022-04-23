;;; memento-mori-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "memento-mori" "memento-mori.el" (0 0 0 0))
;;; Generated autoloads from memento-mori.el

(let ((loads (get 'memento-mori 'custom-loads))) (if (member '"memento-mori" loads) nil (put 'memento-mori 'custom-loads (cons '"memento-mori" loads))))

(defvar memento-mori-birth-date "" "\
*Your birth date in YYYY-MM-DD format.")

(custom-autoload 'memento-mori-birth-date "memento-mori" t)

(defvar memento-mori-mode nil "\
Non-nil if Memento-Mori mode is enabled.
See the `memento-mori-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `memento-mori-mode'.")

(custom-autoload 'memento-mori-mode "memento-mori" nil)

(autoload 'memento-mori-mode "memento-mori" "\
Toggle display of your age in the mode line.

With a prefix argument ARG, enable Memento-Mori mode if ARG is
positive, and disable it otherwise.  If called from Lisp, enable
it if ARG is omitted or nil.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "memento-mori" '("memento-mori-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; memento-mori-autoloads.el ends here
