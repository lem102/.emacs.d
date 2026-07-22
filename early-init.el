(when (eq system-type 'android)
  (setenv "PATH" (format "%s:%s" "/data/data/com.termux/files/usr/bin"
		                 (getenv "PATH")))
  (push "/data/data/com.termux/files/usr/bin" exec-path))

(tool-bar-mode 0)

;; Local Variables:
;; flymake-mode: nil
;; End:
