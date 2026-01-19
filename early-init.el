(tool-bar-mode 0)
(scroll-bar-mode 0)

(when (eq system-type 'android)
  (setenv "PATH" (format "%s:%s" "/data/data/com.termux/files/usr/bin"
		                 (getenv "PATH")))
  (push "/data/data/com.termux/files/usr/bin" exec-path)

  (setopt touch-screen-display-keyboard t))

;; Local Variables:
;; flymake-mode: nil
;; End:
