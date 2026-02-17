;;; jacob-rofi.el --- rofi-like application switcher  -*- lexical-binding: t; -*-


;;; Commentary:
;; 

;;; Code:

(defun jacob-rofi ()
  "Rofi like thing.

Should work on linux and mac. On Linux, wmctrl is used.

1. Create a list of actions from various action sources (e.g. launch
an application, raise an open application, power off the system).
2. Use `completing-read' to select an action.
3. Carry out the selected action."
  (interactive)
  (raise-frame)
  (let* ((actions (append (jacob-rofi--action-source-linux-start-application)
                          ;; (jacob-rofi--action-source-linux-raise-application) ; TODO: to be removed
                          (jacob-rofi--action-source-mac-start-or-raise-application)
                          (jacob-rofi--action-source-system-commands)
                          (jacob-rofi--action-source-org-capture)
                          (jacob-rofi--action-source-fvwm3-commands)))
         (selected-action (cdr (assoc (completing-read "Select action: "
                                                       actions
                                                       nil
                                                       "REQUIRE-MATCH")
                                      actions))))
    (funcall selected-action)))

(defun jacob-rofi--action-source-mac-start-or-raise-application ()
  "An action source for starting or raising applications on mac."
  (when jacob-is-mac
    (let* ((applications (seq-map (lambda (f)
                                    (string-replace ".app" "" f))
                                  (seq-filter (lambda (f)
                                                (string-match-p "\.app" f))
                                              (append (directory-files "/Applications/")
                                                      (directory-files "/System/Applications"))))))
      (seq-map (lambda (application-name)
                 "Return a cons pair of the APPLICATION-NAME of the running application and a function to raise it."
                 (cons (format "Run or raise: %s" application-name)
                       (lambda ()
                         ;; HACK: Selecting emacs makes the call to osascript slow.
                         ;; To improve performance, don't call osascript as Emacs is already focused.
                         (unless (string= application-name "Emacs")
                           (shell-command-to-string (format "osascript -e 'tell application \"%s\" to activate'"
                                                            application-name))))))
               applications))))

(defun jacob-rofi--action-source-linux-start-application ()
  "An action source for starting applications on linux."
  (when jacob-is-linux
    (let* ((applications
            (if jacob-rofi-applications
                jacob-rofi-applications
              (setq jacob-rofi-applications
                    (seq-map #'jacob-rofi-application-constructor
                             (append (directory-files "/usr/share/applications/"
                                                      "FULL"
                                                      "\\.desktop$")
                                     (directory-files "~/.local/share/applications"
                                                      "FULL"
                                                      "\\.desktop$")))))))
      (seq-map (lambda (application)
                 (cons (format "Run or raise: %s" (oref application name))
                       (lambda ()
                         "Run or raise an application."
                         (unless (jacob-rofi-application-raise application)
                           (jacob-rofi-application-run application)))))
               applications))))

;; TODO: this can probably be removed
(defun jacob-rofi--action-source-linux-raise-application ()
  "An action source for raising applications on linux."
  (when jacob-is-linux
    (let* ((ids (string-lines (shell-command-to-string "wmctrl -x -l | awk '{print $1}'")
                              "OMIT-NULLS"))
           (classes (string-lines (shell-command-to-string "wmctrl -x -l | awk '{print $3}'")
                                  "OMIT-NULLS"))
           (titles (string-lines (shell-command-to-string "wmctrl -x -l | awk '{print substr($0, index($0,$5))}'")
                                 "OMIT-NULLS"))
           (id-titles (seq-map (lambda (l)
                                 (cons (format "%s::%s" (cadr l) (caddr l)) (car l)))
                               (cl-mapcar 'list ids classes titles)))
           (actions (seq-map (lambda (pair)
                               "PAIR is (application-name . desktop-file).
Return (application-name . f), where f is a function to raise each application."
                               (cons (format "Raise: %s" (car pair))
                                     (lambda ()
                                       "Raise an application."
                                       (shell-command (format "wmctrl -ia %s" (cdr pair))))))
                             id-titles)))
      actions)))

(defun jacob-rofi--action-source-system-commands ()
  "An action source for controlling the system."
  (list (cons "System Shutdown"
              (lambda ()
                (save-some-buffers t t)
                (run-hook-with-args-until-failure 'kill-emacs-hook)
                (run-hook-with-args-until-failure 'kill-emacs-query-functions)
                (when (yes-or-no-p "Shutdown the system?")
                  (eshell-command (cond (jacob-is-mac "sudo shutdown -h now")
                                        (jacob-is-linux "systemctl poweroff"))))))
        (cons "System Restart"
              (lambda ()
                (save-some-buffers t t)
                (run-hook-with-args-until-failure 'kill-emacs-hook)
                (run-hook-with-args-until-failure 'kill-emacs-query-functions)
                (when (yes-or-no-p "Restart the system?")
                  (eshell-command (cond (jacob-is-mac "sudo shutdown -r")
                                        (jacob-is-linux "systemctl reboot"))))))
        (cons "System Sleep"
              (lambda ()
                (shell-command "systemctl suspend")))
        (cons "System Logout"
              (lambda ()
                (save-some-buffers t t)
                (run-hook-with-args-until-failure 'kill-emacs-hook)
                (run-hook-with-args-until-failure 'kill-emacs-query-functions)
                (when (yes-or-no-p "Logout of the system?")
                  (shell-command "loginctl kill-user `whoami`"))))
        (cons "System Lock"
              (lambda ()
                (eshell-command (cond (jacob-is-mac "pmset displaysleepnow")))))))

(defun jacob-rofi--action-source-fvwm3-commands ()
  "An action source for controlling fvwm."
  (list (cons "Fvwm3 Restart" #'jacob-fvwm-restart)))

(defun jacob-fvwm-restart ()
  "Restart Fvwm."
  (interactive)
  (shell-command "Fvwm3Command Restart"))

(defun jacob-rofi--action-source-org-capture ()
  "An action source for `org-capture'."
  (list (cons "Capture to inbox"
              (lambda ()
                (ignore-errors (org-capture nil "i"))))))

(defvar jacob-rofi-applications '()
  "The applications on the the current X11 system.")

(defclass jacob-rofi-application ()
  ((name :initarg :name
         :initform ""
         :type string
         :documentation "The name of the application.")
   (startup-wmclass :initarg :startup-wmclass
                    :initform nil
                    :type (or null string)
                    :documentation "The StartupWMClass property of an application.")
   (exec :initarg :exec
         :initform ""
         :type string
         :documentation "The cli command to start the application."))
  "Abstration of a desktop application.")

(defun jacob-rofi-application-constructor (file)
  "Produce a application object from a xdg desktop FILE."
  (cl-flet ((get-desktop-file-property (file property)
              (with-temp-buffer
                (insert-file-contents file)
                (goto-char (point-min))
                (re-search-forward (format "^%s=\\(.*\\)$" property) nil "NOERROR")
                (match-string 1))))
    (jacob-rofi-application :name (get-desktop-file-property file "Name")
                            :startup-wmclass (get-desktop-file-property file "StartupWMClass")
                            :exec (string-replace "%u" "" (get-desktop-file-property file "Exec")))))

(cl-defmethod jacob-rofi-application-run ((application jacob-rofi-application))
  "Start a new instance of the APPLICATION."
  (start-process-shell-command "jacob-rofi"
                               nil
                               (oref application exec)))

(cl-defmethod jacob-rofi-application-raise ((application jacob-rofi-application))
  "Raise an x window corresponding to APPLICATION.
Return nil if no window is available."
  (let* ((window-id (with-temp-buffer
                      (insert (shell-command-to-string "wmctrl -x -l"))
                      (goto-char (point-min))
                      (when (re-search-forward (format "%s\\|%s"
                                                       (oref application startup-wmclass)
                                                       (oref application exec))
                                               nil
                                               "NOERROR")
                        (beginning-of-line)
                        (thing-at-point 'word)))))
    (when window-id
      (shell-command (format "wmctrl -ia %s" window-id)))))

(provide 'jacob-rofi)

;;; jacob-rofi.el ends here
