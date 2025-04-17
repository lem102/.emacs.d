;;; csharp-dape.el --- Integration of dape with csharp


;;; Commentary:
;; Provides an `asp-net-launch' profile that "just werks".

(require 'project)
(require 'json)

;;; Code:

(defun jacob-find-launchsettings ()
  "Find launchSettings.json in current project."
  (car (directory-files-recursively (project-root (project-current)) "launchSettings.json$")))

(defun jacob-find-startup-project-directory ()
  "Find startup project of current dotnet solution."
  (locate-dominating-file (jacob-find-launchsettings)
                          (lambda (name)
                            "Return t if file is a directory and contains a `.csproj' file."
                            (when (file-directory-p name)
                              (seq-find (lambda (filename)
                                          "Return t if FILENAME ends with csproj."
                                          (string-match-p ".csproj$" filename))
                                        (directory-files name))))))

(defun jacob-find-dll ()
  "Find the executable path of an asp.net solution's startup dll."
  (let ((project-directory (jacob-find-startup-project-directory)))
    (expand-file-name
     (car (directory-files-recursively project-directory
                                       (concat (file-name-nondirectory
                                                (directory-file-name project-directory))
                                               ".dll"))))))

(defun jacob-get-application-url ()
  "Get the url of the current asp.net project."
  (cdr (assoc "applicationUrl"
              (cdadr (assoc "profiles"
                            (json-read-file (jacob-find-launchsettings))
                            #'string=))
              #'string=)))

(defun dape-asp-net-build-env ()
  "Create the argument for command-env in the `asp-net-launch' profile."
  (list "ASPNETCORE_ENVIRONMENT" "Development"
        "ASPNETCORE_URLS" (jacob-get-application-url)))

(require 'dape)

(add-to-list 'dape-configs '(asp-net-launch modes (csharp-ts-mode)
                                            ensure dape-ensure-command
                                            command "netcoredbg"
                                            command-args ["--interpreter=vscode"]
                                            command-env ("ASPNETCORE_ENVIRONMENT" "Development")
                                            compile "dotnet build"
                                            :request "launch"
                                            :cwd jacob-find-startup-project-directory
                                            :program jacob-find-dll))

;; (launch
;;  modes (csharp-mode csharp-ts-mode)
;;  ensure dape-ensure-command
;;  command "netcoredbg"
;;  command-args ["--interpreter=vscode"]
;;  command-env ("ASPNETCORE_ENVIRONMENT" "Development"
;;               "ASPNETCORE_URLS" "http://localhost:5134")
;;  compile "dotnet build"
;;  :request "launch"
;;  :cwd "/home/jacobl/dev/tappitGitHub/platform-customer-service/Presentation/Customer.Api/"
;;  :program "/home/jacobl/dev/tappitGitHub/platform-customer-service/Presentation/Customer.Api/bin/Debug/net8.0/Customer.Api.dll")

(provide 'csharp-dape)

;;; csharp-dape.el ends here
