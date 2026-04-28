;;; jacob-sm2.el --- Library for interacting with sm2  -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:

(require 'transient)

(defcustom jacob-sm2-config-directory "~/.sm2/service-manager-config/"
  "Directory where sm2 stores it's configuration.")

;;;###autoload
(transient-define-prefix jacob-sm2 ()
  "Transient menu for sm2."
  ["Commands"
   ("i" "Status" jacob-sm2-status)
   ("s" "Start" jacob-sm2-start)
   ("k" "Stop" jacob-sm2-stop)])

(defun jacob-sm2-status ()
  "Run sm2 -s."
  (interactive)
  (async-shell-command "sm2 -s"))

(defun jacob-sm2-start ()
  "Run sm2 --start. Prompt for which service or profile should be started."
  (interactive)
  (async-shell-command (format "sm2 --start %s"
                               (completing-read "Service or profile: "
                                                (jacob-sm2-services-and-profiles)))))

(defun jacob-sm2-stop ()
  "Run sm2 --stop. Prompt for which service or profile should be stopped."
  (interactive)
  (async-shell-command (format "sm2 --stop %s"
                               (completing-read "Service or profile: "
                                                (jacob-sm2-services-and-profiles)))))

(defun jacob-sm2-services-and-profiles ()
  "Get all sm2 services and profiles."
  (append (mapcar #'car
                  (json-read-file (file-name-concat jacob-sm2-config-directory
                                                    "profiles.json")))
          (mapcar #'car
                  (json-read-file (file-name-concat jacob-sm2-config-directory
                                                    "services.json")))
          (mapcar #'car
                  (apply #'append
                         (mapcar #'json-read-file
                                 (directory-files (file-name-concat jacob-sm2-config-directory "services")
                                                  "FULL"
                                                  "^[^.]"))))))

(provide 'jacob-sm2)

;;; jacob-sm2.el ends here
