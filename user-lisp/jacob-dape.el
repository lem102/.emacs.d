;;; jacob-dape.el --- Extras for dape.el  -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:

(require 'eglot)
(require 'jacob-sbt)

;; TODO: investigate whether the "javac -g" flag does something????

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

(defun jacob-dape-metals-attach (config)
  "Setup a metals debugging session in which we attach to sbt.

- Signal to metals via eglot that I want to start a debugging session
- Add the returned uri to CONFIG"
  (let* ((host "localhost")
         (sbt-port (alist-get (sbt:buffer-name) jacob-sbt-port-table nil nil #'string=))
         (session (eglot-execute (eglot-current-server)
                                 (list :command "debug-adapter-start"
                                       :arguments (list :hostName host
                                                        :port sbt-port
                                                        :buildTarget (jacob-sbt-get-project-name)))))
         (debugger-url (url-generic-parse-url (plist-get session :uri)))
         (debugger-port (url-port debugger-url)))
    (seq-reduce (lambda (p c)
                  (plist-put p (cl-first c) (cl-second c)))
                (seq-partition (list 'host host
                                     'port debugger-port
                                     :hostName host
                                     :port sbt-port)
                               2)
                config)))



(provide 'jacob-dape)

;;; jacob-dape.el ends here
