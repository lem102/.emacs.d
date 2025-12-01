;;; jacob-consult.el --- Configuration for consult package  -*- lexical-binding: t; -*-

;;; Commentary:
;; 

;;; Code:

(defun jacob-consult-config ()
  "Apply configuration to the `consult' package."
  (require 'jacob-consult-functions)
  ;; FIXME: Set up autoload to prevent the need to delay binding this command.
  (keymap-set project-prefix-map "g" #'jacob-project-search)
  
  (setopt completion-in-region-function 'consult-completion-in-region
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
         ("i" consult-bookmark)))
       ("e"
        (("s" consult-line)))
       ("k"
        (("u" consult-goto-line)))
       ("j"
        (("g" consult-info)
         ("c" consult-man))))))

    (ryo-modal-keys
     ("SPC"
      (("p" project-prefix-map)))))
  :config (jacob-consult-config))

(provide 'jacob-consult)

;;; jacob-consult.el ends here
