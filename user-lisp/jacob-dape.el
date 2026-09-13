;;; jacob-dape.el --- Extras for dape.el  -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:

(require 'eglot)

(defun jacob-dape-metals-config-function (config)
  "Setup metals debugging session.

- Signal to metals via eglot that I want to start a debugging session
- Add the returned uri to CONFIG"
  (let* ((url (url-generic-parse-url
               (plist-get
                (eglot-execute
                 (eglot-current-server)
                 `( :command "debug-adapter-start"
                    :arguments ( :path ,(eglot-path-to-uri (buffer-file-name))
                                 :runType "runOrTestFile")))
                :uri))))
    (plist-put (plist-put config
                          'host
                          (url-host url))
               'port
               (url-port url))))

(provide 'jacob-dape)

;;; jacob-dape.el ends here
