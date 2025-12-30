;;; jacob-consult.el --- Configuration for consult package  -*- lexical-binding: t; -*-

;;; Commentary:
;; 

;;; Code:

(defun jacob-consult-config ()
  "Apply configuration to the `consult' package."
  (autoload #'jacob-project-search "jacob-consult-functions")
  (autoload #'jacob-consult-buffer-state-no-tramp "jacob-consult-functions")
  
  (setq completion-in-region-function 'consult-completion-in-region
        xref-show-xrefs-function 'consult-xref
        xref-show-definitions-function 'consult-xref
        consult--source-buffer (plist-put consult--source-buffer
                                          :state #'jacob-consult-buffer-state-no-tramp)))

(use-package consult
  :defer t
  :init
  (keymap-global-set "C-x b" #'consult-buffer)
  (keymap-global-set "M-y" #'consult-yank-from-kill-ring)

  (with-eval-after-load "ryo-modal"
    (ryo-modal-keys
     ("SPC"
      (("v" consult-yank-from-kill-ring)
       ("f" consult-buffer)
       ("i"
        (("j" consult-recent-file)
         ("o" consult-bookmark)))
       ("e"
        (("s" consult-line)))
       ("k"
        (("i" consult-register-load)
         ("u" consult-goto-line)))
       ("j"
        (("g" consult-info)
         ("c" consult-man)))))))
  :config
  (jacob-consult-config)

  ;; FIXME: Set up autoload to prevent the need to delay binding this command.
  (keymap-set project-prefix-map "g" #'jacob-project-search)
  ;; due to above binding, we need to reapply the project-prefix-map in ryo-modal-keys
  (with-eval-after-load "ryo-modal"
    (ryo-modal-keys
     ("SPC"
      (("p" project-prefix-map))))))

(provide 'jacob-consult)

;;; jacob-consult.el ends here
