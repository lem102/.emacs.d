(defconst jacob-is-android (eq system-type 'android)
  "Is the current OS android?")

(use-package tool-bar
  :config
  (tool-bar-mode (if jacob-is-android 1 0))
  :custom
  (tool-bar-button-margin (if jacob-is-android 40 4))
  (tool-bar-position (if jacob-is-android 'bottom 'top)))

(use-package menu-bar
  :config
  (menu-bar-mode (if jacob-is-android 1 0)))

(use-package scroll-bar
  :config
  (scroll-bar-mode 0))

(when jacob-is-android
  (setenv "PATH" (format "%s:%s" "/data/data/com.termux/files/usr/bin"
		                 (getenv "PATH")))
  (push "/data/data/com.termux/files/usr/bin" exec-path)

  (setq touch-screen-display-keyboard t))

;; Local Variables:
;; flymake-mode: nil
;; End:
