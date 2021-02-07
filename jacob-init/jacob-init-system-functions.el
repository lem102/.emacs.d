(defun jacob-system-shutdown ()
  "Prompts for yes/no input.

If user inputs yes, system is shutdown. Otherwise, nothing happens."
  (interactive)
  (if (yes-or-no-p "Shutdown system?")
      (shell-command "pwsh -Command Stop-Computer")))

;; https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_pwsh?view=powershell-7.1
