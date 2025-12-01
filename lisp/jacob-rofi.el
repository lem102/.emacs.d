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
                          (jacob-rofi--action-source-linux-raise-application)
                          (jacob-rofi--action-source-mac-start-or-raise-application)
                          (jacob-rofi--action-source-system-commands)))
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
                                                      (directory-files "/System/Applications")))))
           (actions (seq-map (lambda (application-name)
                               "Return a cons pair of the APPLICATION-NAME of the running application and a function to raise it."
                               (cons (format "Run or raise: %s" application-name)
                                     (lambda ()
                                       ;; HACK: Selecting emacs makes the call to osascript slow.
                                       ;; To improve performance, don't call osascript as Emacs is already focused.
                                       (unless (string= application-name "Emacs")
                                         (shell-command-to-string (format "osascript -e 'tell application \"%s\" to activate'"
                                                                          application-name))))))
                             applications)))
      actions)))

(defun jacob-rofi--action-source-linux-start-application ()
  "An action source for starting applications on linux."
  (when jacob-is-linux
    (cl-flet ((get-desktop-file-property (file property)
                (with-temp-buffer
                  (insert-file-contents file)
                  (goto-char (point-min))
                  (re-search-forward (format "^%s=\\(.*\\)$" property))
                  (match-string 1))))
      (let* ((desktop-files
              ;; TODO: filter out desktop files with no wm class
              (seq-filter (lambda (df)
                            (with-temp-buffer
                              (insert-file-contents df)
                              (goto-char (point-min))
                              (search-forward "StartupWMClass" nil "NOERROR")))
                          (append (directory-files "/usr/share/applications/" "FULL" "\\.desktop$")
                                  (directory-files "~/.local/share/applications" "FULL" "\\.desktop$"))))
             (application-names (seq-map (lambda (f)
                                           (get-desktop-file-property f "Name"))
                                         desktop-files))
             (application-classes (seq-map (lambda (f)
                                             (message f)
                                             (get-desktop-file-property f "StartupWMClass"))
                                           desktop-files))
             (application-execs (seq-map (lambda (f)
                                           (string-replace "%u" "" (get-desktop-file-property f "Exec")))
                                         desktop-files))
             (application-data (cl-mapcar #'list desktop-files application-names application-classes application-execs))
             (actions (seq-map (lambda (data)
                                 "DATA is (desktop-file application-name application-class application-exec).
Return (application-name . f), where f is a function to start or raise each application."
                                 (let ((desktop-file (car data))
                                       (name (cadr data))
                                       (class (caddr data))
                                       (exec (cadddr data)))
                                   (cons (format "Start: %s" name)
                                         (lambda ()
                                           "Start an application based on the contents of a .desktop file."
                                           (let* ((wmctrl-window-line (with-temp-buffer
                                                                        (insert (shell-command-to-string "wmctrl -x -l | awk '{print $3}'"))
                                                                        (goto-char (point-min))
                                                                        (when (search-forward class nil "NOERROR")
                                                                          (line-number-at-pos))))
                                                  (window-id (when wmctrl-window-line
                                                               (with-temp-buffer
                                                                 (insert (shell-command-to-string "wmctrl -x -l | awk '{print $1}'"))
                                                                 (goto-line wmctrl-window-line)
                                                                 (buffer-substring (line-beginning-position)
                                                                                   (line-end-position))))))
                                             (if wmctrl-window-line
                                                 (shell-command (format "wmctrl -ia %s" window-id))
                                               (start-process-shell-command "jacob-rofi"
                                                                            nil
                                                                            (string-replace "%u" "" (get-desktop-file-property desktop-file "Exec")))))))))
                               application-data)))
        actions))))

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
                  (eshell-command (cond (jacob-is-mac "sudo shutdown -h")
                                        (jacob-is-linux "systemctl poweroff"))))))
        (cons "System Restart"
              (lambda ()
                (save-some-buffers t t)
                (run-hook-with-args-until-failure 'kill-emacs-hook)
                (run-hook-with-args-until-failure 'kill-emacs-query-functions)
                (when (yes-or-no-p "Restart the system?")
                  (eshell-command (cond (jacob-is-mac "sudo shutdown -r")
                                        (jacob-is-linux "systemctl reboot"))))))
        (cons "System Lock"
              (lambda ()
                (eshell-command (cond (jacob-is-mac "pmset displaysleepnow")))))))

(provide 'jacob-rofi)

;;; jacob-rofi.el ends here
