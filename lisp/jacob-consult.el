;;; jacob-consult.el --- Configuration for consult package

;;; Commentary:
;; 

;;; Code:

(defun jacob-consult-config ()
  "Apply configuration to the `consult' package."
  (require 'jacob-consult-functions)
  (keymap-set project-prefix-map "g" #'jacob-project-search)
  
  (setopt completion-in-region-function 'consult-completion-in-region
          xref-show-xrefs-function 'consult-xref
          xref-show-definitions-function 'consult-xref
          ;; FIXME: broken, see `jacob-consult-buffer-state-no-tramp'
          ;; consult--source-buffer (plist-put consult--source-buffer
          ;; :state #'jacob-consult-buffer-state-no-tramp)
          ))

(use-package consult
  :defer t
  :init
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
         ("c" consult-man)))))))

  (keymap-global-set "C-x b" #'consult-buffer)
  (keymap-global-set "M-y" #'consult-yank-from-kill-ring)
  :config (jacob-consult-config))

(provide 'jacob-consult)

;;; jacob-consult.el ends here
