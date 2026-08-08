;;; jacob-on.el --- Utilities related to the `on' package  -*- lexical-binding: t; -*-

;;; Commentary:
;; The main purpose of code in this file is to provide fault tolerance
;; for the `on' package becoming unavailable.
;;
;; The idea is that wrapper hooks are introduced that we can rely on
;; existing as they are built into the config.
;;
;;`on' is configured so that it's hooks run those wrapper hooks, and
;; the wrapper hooks are used throughout the configuration in place of
;; the `on' hooks. This way, if `on' is not available we can simply
;; eagerly call the wrapper hooks and not miss out on functionality
;; unrelated to `on'.

;;; Code:

(defvar jacob-on-first-file-wrapper-hook nil
  "Wrapper hook for `on-first-file-hook'.

The idea is if the `on' package is unavailable, we can eagerly call this
hook so that functionality outside of `on' is unaffected.

Elsewhere in the init file, do not use `on' directly, instead use this
and similar hooks.")

(defvar jacob-on-first-input-wrapper-hook nil
  "Wrapper hook for `on-first-input-hook'.

The idea is if the `on' package is unavailable, we can eagerly call this
hook so that functionality outside of `on' is unaffected.

Elsewhere in the init file, do not use `on' directly, instead use this
and similar hooks.")

(defvar jacob-on-init-ui-wrapper-hook nil
  "Wrapper hook for `on-init-ui-hook'.

The idea is if the `on' package is unavailable, we can eagerly call this
hook so that functionality outside of `on' is unaffected.

Elsewhere in the init file, do not use `on' directly, instead use this
and similar hooks.")

(defun jacob-run-first-file-wrapper-hook ()
  "Run `jacob-on-first-file-wrapper-hook' hooks."
  (display-warning 'jacob "Function `jacob-run-first-file-wrapper-hook' ran" :debug)
  (run-hooks 'jacob-on-first-file-wrapper-hook))

(defun jacob-run-first-input-wrapper-hook ()
  "Run `jacob-on-first-input-wrapper-hook' hooks."
  (display-warning 'jacob "Function `jacob-run-first-input-wrapper-hook' ran" :debug)
  (run-hooks 'jacob-on-first-input-wrapper-hook))

(defun jacob-run-init-ui-wrapper-hook ()
  "Run `jacob-on-init-ui-wrapper-hook' hooks."
  (display-warning 'jacob "Function `jacob-run-init-ui-wrapper-hook' ran" :debug)
  (run-hooks 'jacob-on-init-ui-wrapper-hook))

(defun jacob-handle-on-unavailable ()
  "Handle `on' being unavailable.

When the package `on' is unavailable, run the wrapper hooks to ensure
functionality outside of `on' is not lost."
  (unless (featurep 'on)
    (display-warning 'jacob "Package `on' unavailable" :warning)
    (jacob-run-first-input-wrapper-hook)
    (jacob-run-first-file-wrapper-hook)
    (jacob-run-init-ui-wrapper-hook)))

(provide 'jacob-on)

;;; jacob-on.el ends here
