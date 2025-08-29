;;; jacob-xah-fly-keys.el --- Configuration for xah-fly-keys

;;; Commentary:
;;

;;; Code:

(defun jacob-xah-fly-keys-initialise ()
  "Initialise `xah-fly-keys'."
  (setq xah-fly-use-control-key nil
        xah-fly-use-meta-key nil))

(defun jacob-xfk-local-key (key command)
  "Bind KEY buffer locally to COMMAND in xfk command mode."
  (let ((existing-command (keymap-lookup xah-fly-command-map key nil "NO-REMAP")))
    (unless existing-command
      (user-error "%s is not bound to a key in `xah-fly-command-map'" key))
    (keymap-local-set (format "<remap> <%s>" existing-command)
                      command)))

(defmacro jacob-xfk-bind-for-mode (mode &rest bindings)
  "Use BINDINGS when in a certain MODE."
  (unless (cl-evenp (length bindings))
    (user-error "`jacob-xfk-bind-for-mode' %s bindings is not a plist"
                mode))
  (let* ((hook (intern (concat (symbol-name mode) "-hook")))
         (hook-function (intern (concat "jacob-" (symbol-name hook) "-function")))
         (binding-alist (seq-reduce (lambda (p c)
                                      "Convert the plist into an alist"
                                      (cond ((and (caar p)
                                                  (null (cdar p)))
                                             (cons (cons (caar p) c)
                                                   (cdr p)))
                                            (t (cons (cons c nil)
                                                     p))))
                                    bindings
                                    '()))
         (hook-function-body (seq-map (lambda (pair)
                                        "PAIR is `'(key command)', return code to bind key to command locally."
                                        (let ((key (car pair))
                                              (command (cdr pair)))
                                          `(let ((existing-command (keymap-lookup xah-fly-command-map
                                                                                  ,key
                                                                                  nil
                                                                                  "NO-REMAP")))
                                             (unless existing-command
                                               (user-error "%s is not bound to a key in `xah-fly-command-map'"
                                                           ,key))
                                             (keymap-local-set (format "<remap> <%s>"
                                                                       existing-command)
                                                               ,command))))
                                      binding-alist)))
    `(progn
       (defun ,hook-function ()
         ,(format "Auto-generated hook function for `%s'." (symbol-name hook))
         ,@hook-function-body)
       (add-hook ',hook #',hook-function))))

(defvar-keymap jacob-xfk-map)

(defun jacob-xah-fly-keys-config ()
  "Configure symbol `xah-fly-keys'."
  (xah-fly-keys 1)

  (xah-fly-keys-set-layout "qwerty")

  (keymap-set xah-fly-leader-key-map "SPC" jacob-xfk-map)
  (keymap-set jacob-xfk-map "p" `("Project" . ,project-prefix-map))

  (defalias 'jacob-return-macro
    (kmacro "<return>"))

  (defvar-keymap jacob-isearch-repeat-map
    :repeat t
    "s" #'isearch-repeat-forward
    "r" #'isearch-repeat-backward)

  (keymap-global-set "<f7>" #'xah-fly-leader-key-map)

  (keymap-set xah-fly-command-map "g" #'expreg-expand)
  ;; (keymap-set xah-fly-command-map "t" #'set-mark-command)
  
  ;; (keymap-set xah-fly-command-map "'" #'jacob-format-words)
  ;; (keymap-set xah-fly-command-map ";" #'jacob-end-of-line)
  (keymap-set xah-fly-command-map "d" #'jacob-backspace)
  (keymap-set xah-fly-command-map "g" #'jacob-kill-paragraph)
  (keymap-set xah-fly-command-map "h" #'jacob-beginning-of-line)
  (keymap-set xah-fly-command-map "s" #'jacob-return-macro)
  (keymap-set xah-fly-command-map "x" #'jacob-kill-line)

  ;; (keymap-set xah-fly-insert-map "M-SPC" #'xah-fly-command-mode-activate)

  ;; (keymap-set xah-fly-leader-key-map ", n" #'jacob-eval-and-replace)
  ;; (keymap-set xah-fly-leader-key-map "/ b" #'vc-switch-branch)
  ;; (keymap-set xah-fly-leader-key-map "/ c" #'vc-create-branch)
  ;; (keymap-set xah-fly-leader-key-map "d i" #'insert-pair)
  ;; (keymap-set xah-fly-leader-key-map "d j" #'insert-pair)
  ;; (keymap-set xah-fly-leader-key-map "d k" #'insert-pair)
  ;; (keymap-set xah-fly-leader-key-map "d l" #'insert-pair)
  ;; (keymap-set xah-fly-leader-key-map "d u" #'insert-pair)
  ;; (keymap-set xah-fly-leader-key-map "i i" #'consult-bookmark)
  ;; (keymap-unset xah-fly-leader-key-map "i o") ; `bookmark-jump'
  ;; (keymap-unset xah-fly-leader-key-map "i p") ; `bookmark-set'
  ;; (keymap-set xah-fly-leader-key-map "l 3" #'jacob-async-shell-command)
  ;; (keymap-set xah-fly-leader-key-map "l a" #'global-text-scale-adjust)

  (keymap-set xah-fly-leader-key-map "w j" #'xref-find-references)
  (keymap-set xah-fly-leader-key-map "w l" #'xref-go-back)
  )

(use-package xah-fly-keys
  :ensure t
  :demand
  :init
  (jacob-xah-fly-keys-initialise)
  :config
  (jacob-xah-fly-keys-config))

(provide 'jacob-xah-fly-keys)

;;; jacob-xah-fly-keys.el ends here
