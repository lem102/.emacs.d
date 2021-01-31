(use-package eglot
  :ensure t
  :defer t
  :config
  (defconst jacob-eclipse-jdt-home "/home/lem/Documents/java/java-language-server/plugins/org.eclipse.equinox.launcher_1.5.700.v20200207-2156.jar")

  (defun jacob-eglot-eclipse-jdt-contact (interactive)
    "Contact with the jdt server input INTERACTIVE."
    (let ((cp (getenv "CLASSPATH")))
      (setenv "CLASSPATH" (concat cp ":" jacob-eclipse-jdt-home))
      (unwind-protect (eglot--eclipse-jdt-contact nil)
        (setenv "CLASSPATH" cp))))
  (setcdr (assq 'java-mode eglot-server-programs) #'jacob-eglot-eclipse-jdt-contact))
