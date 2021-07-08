(jacob-is-installed 'eglot
  (add-hook 'java-mode-hook 'eglot-ensure)
  (add-hook 'csharp-mode-hook 'eglot-ensure)
  (add-hook 'js-mode-hook 'eglot-ensure)
  (add-hook 'typescript-mode-hook 'eglot-ensure)
  (with-eval-after-load 'eglot
    (setcdr (assq 'java-mode eglot-server-programs) #'jacob-eglot-eclipse-jdt-contact)

    (add-to-list 'eglot-server-programs
                 `(csharp-mode . ("d:/programming/OmniSharp/omnisharp-win-x64/OmniSharp.exe" "-lsp")))

    (defun jacob-eglot-eclipse-jdt-contact
        (interactive)
      "Contact with the jdt server input INTERACTIVE."
      (let ((cp (getenv "CLASSPATH"))
            (jdt-home jacob-eclipse-jdt-file-path))
        (setenv "CLASSPATH" (concat cp ":" jdt-home))
        (unwind-protect (eglot--eclipse-jdt-contact nil)
          (setenv "CLASSPATH" cp))))

    (defun eglot--make-diag (buffer
                             beg
                             end
                             type
                             text
                             &optional data
                             overlay-properties)
      "Make a Flymake diagnostic for BUFFER's region from BEG to END.
TYPE is a diagnostic symbol and TEXT is string describing the
problem detected in this region.  DATA is any object that the
caller wishes to attach to the created diagnostic for later
retrieval.
 
OVERLAY-PROPERTIES is an alist of properties attached to the
created diagnostic, overriding the default properties and any
properties of `flymake-overlay-control' of the diagnostic's
type."
      (if (not (string= text "typescript: Experimental support for decorators is a feature that is subject to change in a future release. Set the 'experimentalDecorators' option in your 'tsconfig' or 'jsconfig' to remove this warning."))
          (flymake--diag-make :buffer buffer :beg beg :end end
                              :type type :text text :data data
                              :overlay-properties overlay-properties)
        (flymake--diag-make :buffer buffer :beg 0 :end 0
                            :type type :text text :data data
                            :overlay-properties overlay-properties)))))
