;;; jacob-sm2.el --- Library for interacting with sm2  -*- lexical-binding: t; -*-

;;; Commentary:

;; TODO: handle colima start as well?

;; TODO: handle append args

;;; Code:

(require 'transient)

(defcustom jacob-sm2-config-directory "~/.sm2/service-manager-config/"
  "Directory where sm2 stores it's configuration.")

;;;###autoload
(transient-define-prefix jacob-sm2 ()
  "Transient menu for sm2."
  ["Commands"
   ("i" "Status" jacob-sm2-status)
   ("s" "Start service or profile" jacob-sm2-start)
   ("c" "Clean then start service or profile" jacob-sm2-start-clean)
   ("h" "See help" jacob-sm2-help)
   ("k" "Stop service or profile" jacob-sm2-stop)
   ("l" "View Logs for service" jacob-sm2-logs)
   ("p" "Prune services" jacob-sm2-prune)
   ("us" "Update sm2" jacob-sm2-update)
   ("uc" "Update sm2 config" jacob-sm2-update-config)
   ("q" "Quit" ignore)])

(defun jacob-sm2-status ()
  "Run sm2 -s."
  (interactive)
  (async-shell-command "sm2 -s"))

(defun jacob-sm2-help ()
  "Run sm2 --help."
  (interactive)
  (async-shell-command "sm2 --help"))

(defun jacob-sm2-prune ()
  "Run sm2 --prune."
  (interactive)
  (async-shell-command "sm2 --prune"))

(defun jacob-sm2-start ()
  "Run sm2 --start. Prompt for which service or profile should be started."
  (interactive)
  (let* (

         ;; Here we tell emacs to use a "pipe" instead of using a
         ;; "pty". If a "pty" is used, the process created by sm2 will
         ;; exit with no obvious errors straight away after it is
         ;; created, according to the logs. My current line of
         ;; reasoning is that when a "pty" is closed it does some kind
         ;; of cleanup including the service sm2 is starting! A "pipe"
         ;; does not have this issue. Will hopefully update this
         ;; comment when I know what the heck I'm doing in this area.
         (process-connection-type nil)

         )
    (async-shell-command (format "sm2 --workers 4 --start %s"
                                 (completing-read "Start service or profile: "
                                                  (jacob-sm2--get-services "AND-PROFILES"))))))

(defun jacob-sm2-start-clean ()
  "Run sm2 --clean --start. Prompt for which service or profile should be started."
  (interactive)
  (let* ((process-connection-type nil))
    (async-shell-command (format "sm2 --workers 4 --clean --start %s"
                                 (completing-read "Start service or profile: "
                                                  (jacob-sm2--get-services "AND-PROFILES"))))))

(defun jacob-sm2-stop ()
  "Run sm2 --stop. Prompt for which service or profile should be stopped."
  (interactive)
  (async-shell-command (format "sm2 --stop %s"
                               (completing-read "Stop service or profile: "
                                                (jacob-sm2--get-services "AND-PROFILES")))))

(defun jacob-sm2-logs ()
  "View logs for a service."
  (interactive)
  (async-shell-command (format "sm2 --logs %s"
                               (completing-read "View logs for service: "
                                                (jacob-sm2--get-services)))))

(defun jacob-sm2-update ()
  "Run sm2 --update."
  (interactive)
  (async-shell-command "sm2 --update"))

(defun jacob-sm2-update-config ()
  "Run sm2 --update-config."
  (interactive)
  (async-shell-command "sm2 --update-config"))

(defun jacob-sm2--get-services (&optional and-profiles)
  "Get all sm2 services. If AND-PROFILES is non-nil, also return profiles."
  (append (when and-profiles
            (mapcar #'car
                    (json-read-file (file-name-concat jacob-sm2-config-directory
                                                      "profiles.json"))))
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
