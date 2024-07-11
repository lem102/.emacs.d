;;; activities-autoloads.el --- automatically extracted autoloads (do not edit)   -*- lexical-binding: t -*-
;; Generated by the `loaddefs-generate' function.

;; This file is part of GNU Emacs.

;;; Code:

(add-to-list 'load-path (or (and load-file-name (directory-file-name (file-name-directory load-file-name))) (car load-path)))



;;; Generated autoloads from activities.el

(autoload 'activities-define "activities" "\
Define current state as a new activity with NAME.
If FORCEP (interactively, with prefix), redefine existing
activity.

(fn NAME &key FORCEP)" t)
(autoload 'activities-new "activities" "\
Switch to a newly defined activity named NAME.

(fn NAME)" t)
(autoload 'activities-resume "activities" "\
Resume ACTIVITY.
If RESETP (interactively, with universal prefix), reset to
ACTIVITY's default state; otherwise, resume its last state, if
available.

(fn ACTIVITY &key RESETP)" t)
(defvar activities-mode nil "\
Non-nil if Activities mode is enabled.
See the `activities-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `activities-mode'.")
(custom-autoload 'activities-mode "activities" nil)
(autoload 'activities-mode "activities" "\
Automatically remember activities' state.

accordingly.

This is a global minor mode.  If called interactively, toggle the
`Activities mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value \\='activities-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t)
(register-definition-prefixes "activities" '("activities-"))


;;; Generated autoloads from activities-list.el

(autoload 'activities-list "activities-list" "\
List activities." t)
(defalias 'list-activities #'activities-list)
(register-definition-prefixes "activities-list" '("activities-list-"))


;;; Generated autoloads from activities-tabs.el

(defvar activities-tabs-mode nil "\
Non-nil if Activities-Tabs mode is enabled.
See the `activities-tabs-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `activities-tabs-mode'.")
(custom-autoload 'activities-tabs-mode "activities-tabs" nil)
(autoload 'activities-tabs-mode "activities-tabs" "\
Integrate Activities with `tab-bar-mode'.

When active, activities are opened in new tabs and named
accordingly.

This is a global minor mode.  If called interactively, toggle the
`Activities-Tabs mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value \\='activities-tabs-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t)
(register-definition-prefixes "activities-tabs" '("activities-tabs-"))

;;; End of scraped data

(provide 'activities-autoloads)

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; no-native-compile: t
;; coding: utf-8-emacs-unix
;; End:

;;; activities-autoloads.el ends here