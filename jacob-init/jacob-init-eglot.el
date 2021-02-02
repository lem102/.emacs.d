(use-package eglot
  :ensure t
  :config
  (defun jacob-eglot-eclipse-jdt-contact
      (interactive)
    "Contact with the jdt server input INTERACTIVE."
    (let ((cp (getenv "CLASSPATH"))
          (jdt-home jacob-eclipse-jdt-file-path))
      (setenv "CLASSPATH" (concat cp ":" jdt-home))
      (unwind-protect (eglot--eclipse-jdt-contact nil)
        (setenv "CLASSPATH" cp))))
  
  (setcdr (assq 'java-mode eglot-server-programs) #'jacob-eglot-eclipse-jdt-contact)
  
  :hook (java-mode-hook . eglot-ensure))
