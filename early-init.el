(defconst jacob-is-android (eq system-type 'android)
  "Is the current OS android?")

(tool-bar-mode (if jacob-is-android 1 0))

(setopt tool-bar-button-margin (if jacob-is-android 40 4)
        tool-bar-position (if jacob-is-android 'bottom 'top))

(menu-bar-mode (if jacob-is-android 1 0))

(scroll-bar-mode 0)

(when jacob-is-android
  (setenv "PATH" (format "%s:%s" "/data/data/com.termux/files/usr/bin"
		                 (getenv "PATH")))
  (push "/data/data/com.termux/files/usr/bin" exec-path)

  (setopt touch-screen-display-keyboard t))

;; Local Variables:
;; flymake-mode: nil
;; End:
