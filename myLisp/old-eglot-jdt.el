;;; Setup specific to the Eclipse JDT setup in case one can't use the simpler 'jdtls' script


(with-eval-after-load 'eglot


  (defun jacob-eglot-eclipse-jdt-contact
      (interactive)
    "Contact with the jdt server input INTERACTIVE."
    (let ((cp (getenv "CLASSPATH"))
          (jdt-home jacob-eclipse-jdt-file-path))
      (setenv "CLASSPATH" (concat cp path-separator jdt-home))
      (unwind-protect (eglot--eclipse-jdt-contact nil)
        (setenv "CLASSPATH" cp))))
  
  ;; Tell Eglot to use a specific class to handle java-mode files
  (add-to-list 'eglot-server-programs '(java-mode . jacob-eglot-eclipse-jdt-contact))

  (defun eglot--eclipse-jdt-contact (interactive)
    "Return cons (CLASS . ARGS) for connecting to Eclipse JDT.
If INTERACTIVE, prompt user for details."
    (cl-labels
        ((is-the-jar
          (path)
          (and (string-match-p
                "org\\.eclipse\\.equinox\\.launcher_.*\\.jar$"
                (file-name-nondirectory path))
               (file-exists-p path))))
      (let* ((classpath (or (getenv "CLASSPATH") path-separator))
             (cp-jar (cl-find-if #'is-the-jar (split-string classpath path-separator)))
             (jar cp-jar)
             (dir
              (cond
               (jar (file-name-as-directory
                     (expand-file-name ".." (file-name-directory jar))))
               (interactive
                (expand-file-name
                 (read-directory-name
                  (concat "Path to eclipse.jdt.ls directory (could not"
                          " find it in CLASSPATH): ")
                  nil nil t)))
               (t (error "Could not find eclipse.jdt.ls jar in CLASSPATH"))))
             (repodir
              (concat dir
                      "org.eclipse.jdt.ls.product/target/repository/"))
             (repodir (if (file-directory-p repodir) repodir dir))
             (config
              (concat
               repodir
               (cond
                ((string= system-type "darwin") "config_mac")
                ((string= system-type "windows-nt") "config_win")
                (t "config_linux"))))
             (workspace
              (expand-file-name (md5 (project-root (eglot--current-project)))
                                (locate-user-emacs-file
                                 "eglot-eclipse-jdt-cache"))))
        (unless jar
          (setq jar
                (cl-find-if #'is-the-jar
                            (directory-files (concat repodir "plugins") t))))
        (unless (and jar (file-exists-p jar) (file-directory-p config))
          (error "Could not find required eclipse.jdt.ls files (build required?)"))
        (when (and interactive (not cp-jar)
                   (y-or-n-p (concat "Add path to the server program "
                                     "to CLASSPATH environment variable?")))
          (setenv "CLASSPATH" (concat (getenv "CLASSPATH") path-separator jar)))
        (unless (file-directory-p workspace)
          (make-directory workspace t))
        (cons 'eglot-eclipse-jdt
              (list (executable-find "java")
                    "-Declipse.application=org.eclipse.jdt.ls.core.id1"
                    "-Dosgi.bundles.defaultStartLevel=4"
                    "-Declipse.product=org.eclipse.jdt.ls.core.product"
                    "-jar" jar
                    "-configuration" config
                    "-data" workspace)))))

  ;; Define said class and its methods
  (defclass eglot-eclipse-jdt (eglot-lsp-server) ()
    :documentation "Eclipse's Java Development Tools Language Server.")

  (cl-defmethod eglot-initialization-options ((server eglot-eclipse-jdt))
    "Passes through required JDT initialization options."
    `(:workspaceFolders
      [,@(cl-delete-duplicates
          (mapcar #'eglot--path-to-uri
                  (let* ((root (project-root (eglot--project server))))
                    (cons root
                          (mapcar
                           #'file-name-directory
                           (append
                            (file-expand-wildcards (concat root "*/pom.xml"))
                            (file-expand-wildcards (concat root "*/build.gradle"))
                            (file-expand-wildcards (concat root "*/.project")))))))
          :test #'string=)]
      ,@(if-let ((home (or (getenv "JAVA_HOME")
                           (ignore-errors
                             (expand-file-name
                              ".."
                              (file-name-directory
                               (file-chase-links (executable-find "javac"))))))))
            `(:settings (:java (:home ,home)))
          (ignore (eglot--warn "JAVA_HOME env var not set")))))

  (cl-defmethod eglot-execute-command
    ((_server eglot-eclipse-jdt) (_cmd (eql java.apply.workspaceEdit)) arguments)
    "Eclipse JDT breaks spec and replies with edits as arguments."
    (mapc #'eglot--apply-workspace-edit arguments)))
