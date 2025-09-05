;;; jacob-xah-fly-keys.el --- Configuration for xah-fly-keys

;;; Commentary:
;;

;;; Code:

(defun jacob-xah-fly-keys-initialise ()
  "Initialise `xah-fly-keys'."
  (setq xah-fly-use-control-key nil
        xah-fly-use-meta-key nil))

(defvar-keymap jacob-xfk-map)

(defun jacob-xah-fly-keys-config ()
  "Configure symbol `xah-fly-keys'."
  (xah-fly-keys 1)

  (xah-fly-keys-set-layout "qwerty")

  (keymap-set xah-fly-leader-key-map "SPC" jacob-xfk-map)
  (keymap-set jacob-xfk-map "p" `("Project" . ,project-prefix-map))

  (require 'jacob-xah-fly-keys-functions)

  (defvar-keymap jacob-isearch-repeat-map
    :repeat t
    "s" #'isearch-repeat-forward
    "r" #'isearch-repeat-backward)

  (keymap-global-set "<f7>" #'xah-fly-leader-key-map)
  (keymap-global-set "M-SPC" #'xah-fly-command-mode-activate)

  (keymap-set xah-fly-command-map "g" #'expreg-expand)

  (keymap-set xah-fly-command-map "t" #'set-mark-command)
  (keymap-set xah-fly-leader-key-map "s" #'exchange-point-and-mark)
  
  ;; (keymap-set xah-fly-command-map "'" #'jacob-format-words)
  (keymap-set xah-fly-command-map ";" #'jacob-end-of-line)
  (keymap-set xah-fly-command-map "d" #'jacob-backspace)
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
  (keymap-unset xah-fly-leader-key-map "i o") ; `bookmark-jump'
  (keymap-unset xah-fly-leader-key-map "i p") ; `bookmark-set'
  ;; (keymap-set xah-fly-leader-key-map "l 3" #'jacob-async-shell-command)
  ;; (keymap-set xah-fly-leader-key-map "l a" #'global-text-scale-adjust)

  (keymap-set xah-fly-leader-key-map "w j" #'xref-find-references)
  (keymap-set xah-fly-leader-key-map "w l" #'xref-go-back)
  )

(use-package xah-fly-keys
  :ensure t
  :demand
  :blackout (xah-fly-keys . " 🛪")
  :init
  (jacob-xah-fly-keys-initialise)
  :config
  (jacob-xah-fly-keys-config))

(provide 'jacob-xah-fly-keys)

;;; jacob-xah-fly-keys.el ends here
