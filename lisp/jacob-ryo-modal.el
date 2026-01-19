;;; jacob-ryo-modal.el --- ryo-modal setup -*- lexical-binding: t; -*-

;;; Commentary:
;; setup for ryo-modal


;;; Code:

(require 'ryo-modal)
(require 'winner)
(require 'jacob-init-helpers)
(require 'jacob-editing-commands)

(define-global-minor-mode global-ryo-modal-mode ryo-modal-mode
  (lambda ()
    (ryo-modal-mode 1)))

(defun global-ryo-modal-mode-refresh-keys ()
  "\"Turn it off and on again\", to ensure keys are correct in current buffer."
  (global-ryo-modal-mode-disable)
  (global-ryo-modal-mode-enable))

(defun global-ryo-modal-mode-enable ()
  "Enable `global-ryo-modal-mode'."
  (interactive)
  (unless global-ryo-modal-mode
    (global-ryo-modal-mode 1)))

(keymap-global-set "M-SPC" #'global-ryo-modal-mode-enable)
(add-hook 'minibuffer-exit-hook #'global-ryo-modal-mode-enable)

(defun global-ryo-modal-mode-disable ()
  "Disable `global-ryo-modal-mode'."
  (interactive)
  (when global-ryo-modal-mode
    (global-ryo-modal-mode 0)))

(add-hook 'minibuffer-setup-hook #'global-ryo-modal-mode-disable)

(jacob-defhookf global-ryo-modal-mode-hook
  (global-hl-line-mode (if global-ryo-modal-mode 1 0))
  (modify-all-frames-parameters (list (cons 'cursor-type
                                            (if global-ryo-modal-mode
                                                'box
                                              'bar)))))

;; HACK: Patch ryo-modal-mode to revert changes to cursor colour.
(defun jacob-ryo-modal-fix-cursor (&rest _args)
  "Revert the colour of the cursor to the current theme's default."
  (remove-hook 'post-command-hook #'ryo-modal--cursor-color-update)
  (custom-theme-recalc-face 'cursor))

(advice-add #'ryo-modal-mode :after #'jacob-ryo-modal-fix-cursor)

(ryo-modal-keys
 ("j" backward-char)
 ("k" next-line)
 ("i" previous-line)
 ("l" forward-char)

 ("u" backward-word)
 ("o" forward-word)

 ("h" jacob-beginning-of-line)
 (";" jacob-end-of-line)

 ("y" undo)
 ("n" isearch-forward)
 ("," other-window)

 ("m" jacob-backward-sexp)
 ("." jacob-forward-sexp)

 ("a" execute-extended-command)
 ("s" jacob-return-macro)
 ("d" jacob-backspace)
 ("f" global-ryo-modal-mode-disable)

 ("w" xah-shrink-whitespaces)
 ("e" backward-kill-word)
 ("r" kill-word)

 ("t" set-mark-command)

 ("p" recenter-top-bottom)

 ("x" jacob-kill-line)
 ("c" jacob-copy-line-or-region)
 ("v" yank)

 ("z" xah-comment-dwim)
 ("b" xah-toggle-letter-case)

 ("'" delete-other-windows)
 ("-" split-window-below)
 ("=" split-window-right)

 ("1" winner-undo)
 ("2" winner-redo)
 ("4" other-window-prefix)
 ("5" delete-forward-char)
 ("6" jacob-mark-paragraph)
 ("7" jacob-mark-line)
 ("0" pop-to-mark-command)

 ("SPC"
  (("'" delete-window)
   (";" save-buffer)
   ("." universal-argument)
   ("b" xah-toggle-previous-letter-case)
   ("c" jacob-copy-buffer)
   ("e"
    (("e" highlight-symbol-at-point)
     ("u" unhighlight-regexp)))
   ("m" dired-jump)
   ("n" end-of-buffer)
   ("h" beginning-of-buffer)
   ("p" project-prefix-map)
   ("y" isearch-forward-symbol-at-point)
   (","
    (("," delete-frame)
     ("d" eval-defun)
     ("m" eval-last-sexp)
     ("x" save-buffers-kill-emacs)
     ("r" eval-expression)))
   ("s" exchange-point-and-mark)
   ("j"
    (("c" man)
     ("y" describe-face)
     ("i" describe-char)
     ("g" info)))
   ("l"
    (("6" calendar)
     ("a" global-text-scale-adjust)
     (";" global-display-line-numbers-mode)
     ("d" eshell)
     ("e" toggle-frame-maximized)
     ("f" shell)
     ("g" make-frame-command)
     ("o" count-words)
     ("n" toggle-debug-on-error)
     ("i" toggle-case-fold-search)))
   ("g"
    (("h" kill-paragraph)
     ("j" mark-paragraph)
     ))
   ("o"
    (("d" kmacro-call-macro)
     ("e" kmacro-start-macro)
     ("h" delete-rectangle)
     ("i" string-rectangle)
     ("l" rectangle-number-lines)
     ("o" rectangle-mark-mode)
     ("p" clear-rectangle)
     ("r" kmacro-end-macro)
     ("s" open-rectangle)
     ("v" yank-rectangle)
     ("x" kill-rectangle)))
   ("i"
    (("e" find-file)))
   ("k"
    (("c" copy-to-register)
     ("k" repeat)))
   ("u" kill-current-buffer)
   ("w"
    (("j" xref-find-references)
     ("k" xref-find-definitions)
     ("l" xref-go-back)))
   ("/"
    (("j" vc-diff)
     ("h" vc-annotate))))))

(add-hook 'after-init-hook #'global-ryo-modal-mode-enable)

;; isearch

(keymap-set isearch-mode-map "<right>" #'isearch-repeat-forward)
(keymap-set isearch-mode-map "<left>" #'isearch-repeat-backward)

;; TODO: adapt the below code

;; (defvar-keymap jacob-code-map
;;     "e" #'eglot
;;     "a" #'eglot-code-actions
;;     "r" #'eglot-rename
;;     "i" #'eglot-find-implementation
;;     "t" #'eglot-find-typeDefinition)

;;   (with-eval-after-load "ryo-modal"
;;     (keymap-set jacob-xfk-map "c" `("Code" . ,jacob-code-map)))

;; (defvar-keymap jacob-org-agenda-map
;;   "a" #'org-agenda
;;   "c" #'org-capture)

;; (with-eval-after-load "ryo-modal"
;;   (keymap-set jacob-xfk-map "a" `("Agenda" . ,jacob-org-agenda-map)))

;; (jacob-xfk-bind-for-mode sql-interactive-mode
;;                            "SPC , d" #'sql-send-paragraph)


(provide 'jacob-ryo-modal)

;;; jacob-ryo-modal.el ends here
