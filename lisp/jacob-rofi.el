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
                          (jacob-rofi--action-source-mac-start-or-raise-application)))
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
    (let* ((desktop-files (append (directory-files "/usr/share/applications/" "FULL" "\\.desktop$")
                                  (directory-files "~/.local/share/applications" "FULL" "\\.desktop$")))
           (application-names (seq-map (lambda (f)
                                         (with-temp-buffer
                                           (insert-file-contents f)
                                           (goto-char (point-min))
                                           (re-search-forward "^Name=")
                                           (buffer-substring (point) (line-end-position))))
                                       desktop-files))
           (application-alist (cl-mapcar #'cons application-names desktop-files))
           (actions (seq-map (lambda (pair)
                               "PAIR is (application-name . desktop-file).
Return (application-name . f), where f is a function to start each application."
                               (cons (format "Start: %s" (car pair))
                                     (lambda ()
                                       "Start an application based on the contents of a .desktop file."
                                       (start-process-shell-command "jacob-rofi"
                                                                    nil
                                                                    (with-temp-buffer
                                                                      (insert-file-contents (cdr pair))
                                                                      (goto-char (point-min))
                                                                      (re-search-forward "^Exec=\\(.*\\)$")
                                                                      (replace-regexp-in-string "%u" "" (match-string 1)))))))
                             application-alist)))
      actions)))

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

(defun jacob-system-shutdown ()
  "Shutdown the system."
  (interactive)
  (start-process-shell-command "jacob-system"
                               nil
                               (cond (jacob-is-mac "shutdown -h +1"))))

(provide 'jacob-rofi)

;;; jacob-rofi.el ends here
