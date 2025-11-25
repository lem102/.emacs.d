;;; jacob-ryo-modal.el --- ryo-modal setup -*- lexical-binding: t; -*-

;;; Commentary:
;; setup for ryo-modal


;;; Code:

(require 'ryo-modal)
(require 'eglot)
(require 'winner)
(require 'jacob-xah-fly-keys-functions)

(setq ryo-modal-cursor-color nil)

(define-global-minor-mode global-ryo-modal-mode ryo-modal-mode
  (lambda ()
    (ryo-modal-mode 1)))

(add-hook 'minibuffer-setup-hook #'global-ryo-modal-mode-disable)
(add-hook 'minibuffer-exit-hook #'global-ryo-modal-mode-enable)

(defun global-ryo-modal-mode-enable ()
  "Enable `global-ryo-modal-mode'."
  (interactive)
  (unless global-ryo-modal-mode
    (global-ryo-modal-mode 1)))

(defun global-ryo-modal-mode-disable ()
  "Disable `global-ryo-modal-mode'."
  (interactive)
  (when global-ryo-modal-mode
    (global-ryo-modal-mode 0)))

(keymap-global-set "M-SPC" #'global-ryo-modal-mode-enable)

(jacob-defhookf global-ryo-modal-mode-hook
  (global-hl-line-mode (if global-ryo-modal-mode 1 0))
  (modify-all-frames-parameters (list (cons 'cursor-type
                                            (if global-ryo-modal-mode
                                                'box
                                              'bar)))))

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

 ("g" expreg-expand)

 ("w" jacob-delete-whitespace)
 ("e" backward-kill-word)
 ("r" kill-word)

 ("t" set-mark-command)

 ("p" recenter-top-bottom)

 ("x" jacob-kill-line)
 ("c" jacob-copy-line-or-region)
 ("v" yank)

 ("z" jacob-comment)
 ("b" jacob-toggle-word-case)

 ("\\" embark-act)
 ("'" delete-other-windows)

 ("1" winner-undo)
 ("2" winner-redo)
 ("3" other-window-prefix)
 ("4" split-window-right)
 ("5" delete-forward-char)
 ("6" jacob-mark-paragraph)
 ("7" jacob-mark-line)
 ("0" pop-to-mark-command)

 ("SPC"
  (("3" delete-window)
   ("4" split-window-below)
   (";" save-buffer)
   ("." universal-argument)
   ;; TODO: replace
   ;; ("b" xah-toggle-previous-letter-case)
   ("c" jacob-copy-buffer)
   ("e"
    (("s" consult-line)
     ("e" highlight-symbol-at-point)
     ("u" unhighlight-regexp)))
   ("m" dired-jump)
   ("n" end-of-buffer)
   ("h" beginning-of-buffer)
   ("p" project-prefix-map)
   ("y" isearch-forward-symbol-at-point)
   ("/"
    (("m" magit-project-status)
     ("h" vc-annotate)))
   (","
    (("d" eval-defun)
     ("m" eval-last-sexp)
     ("x" save-buffers-kill-emacs)
     ("r" eval-expression)))
   ("f" consult-buffer)
   ("s" exchange-point-and-mark)
   ("j"
    (("k" helpful-callable)
     ("l" helpful-variable)
     ("v" helpful-key)
     ("y" describe-face)
     ("i" describe-char)
     ("g" info)))
   ("l"
    (("a" global-text-scale-adjust)
     ("e" toggle-frame-maximized)
     ("d" eshell)
     ("o" count-words)))
   ("g"
    (("h" kill-paragraph)
     ("j" mark-paragraph)
     ))
   ("o"
    (("e" kmacro-start-macro)
     ("r" kmacro-end-macro)
     ("d" kmacro-call-macro)
     ("i" string-rectangle)
     ("h" delete-rectangle)))
   ("i"
    (("e" find-file)
     ("o" consult-bookmark)))
   ("k"
    (("u" consult-goto-line)))
   ("r" visual-replace)
   ("u" kill-this-buffer)
   ("w"
    (("k" xref-find-definitions)
     ("l" xref-go-back)))
   ("v" consult-yank-from-kill-ring)
   ;; move these out of the SPC SPC area
   ("SPC"
    (("c"
      (("e" eglot)
       ("r" eglot-rename))))
    (("y"
      (("n" yas-new-snippet)
       ("v" yas-visit-snippet-file))))))))

(add-hook 'after-init-hook #'global-ryo-modal-mode-enable)

;; isearch

(keymap-set isearch-mode-map "<right>" #'isearch-repeat-forward)
(keymap-set isearch-mode-map "<left>" #'isearch-repeat-backward)

(provide 'jacob-ryo-modal)

;;; jacob-ryo-modal ends here
