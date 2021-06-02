(progn
  (require 'orderless)
  (setq completion-styles '(orderless)))

(progn
  (require 'prescient)
  (prescient-persist-mode 1))

(progn
  (require 'selectrum-prescient)
  (selectrum-prescient-mode 1))

(progn
  (require 'marginalia)
  (marginalia-mode 1))

(progn
  (require 'selectrum)
  (setq selectrum-display-action nil)
  (selectrum-mode 1))

(progn
  (require 'consult)
  (setq completion-in-region-function 'consult-completion-in-region)
  (setq consult-preview-key nil)        ; TODO: fine tune this later
  (global-set-key (kbd "C-x b") 'consult-buffer)
  (global-set-key (kbd "C-z n") 'consult-line)
  (define-key xah-fly-dot-keymap (kbd "s") 'consult-line)
  (define-key xah-fly-c-keymap (kbd "j") 'consult-recent-file)
  (define-key xah-fly-leader-key-map (kbd "v") 'consult-yank))
