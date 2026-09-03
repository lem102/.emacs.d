;;; jacob-modal-editing-config.el --- configuration for `jacob-modal-editing' -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:

(defun jme-bind-keys (map bindings)
  "Bind multiple keys in MAP. BINDINGS is an alist of (key . command)."
  (dolist (binding bindings)
    (keymap-set map (car binding) (cdr binding))))

(defun jme-major-mode-override-keys (mode bindings)
  "Define a keymap for MODE with BINDINGS and add it to `jacob-modal-editing-overriding-map-alist'."
  (let ((map (make-sparse-keymap)))
    (dolist (binding bindings)
      (keymap-set map (car binding) (cdr binding)))
    (add-to-list 'jme-overriding-map-alist (cons mode map))))

;; TODO: t and g have been combined, t is now a free key

(jme-bind-keys jme-command-mode-map
               `(("," . jacob-split-or-switch-window)
                 ("-" . split-window-below)
                 ("." . puni-forward-sexp-or-up-list)
                 ("0" . pop-to-mark-command)
                 ("1" . winner-undo)
                 ("2" . winner-redo)
                 ("4" . other-window-prefix)
                 ("5" . delete-forward-char)
                 ("6" . jacob-mark-paragraph)
                 ("7" . jacob-mark-line)
                 (";" . jacob-end-of-line)
                 ("=" . split-window-right)
                 ("@" . delete-window)
                 ("N" . isearch-backward)
                 ("SPC '" . delete-window)
                 ("SPC , ," . delete-frame)
                 ("SPC , d" . eval-defun)
                 ("SPC , e" . eval-buffer)
                 ("SPC , f" . eval-region)
                 ("SPC , m" . eval-last-sexp)
                 ("SPC , n" . jacob-eval-and-replace)
                 ("SPC , r" . eval-expression)
                 ("SPC , x" . save-buffers-kill-emacs)
                 ("SPC ." . universal-argument)
                 ("SPC / h" . vc-annotate)
                 ("SPC / i" . vc-update)
                 ("SPC / j" . vc-diff)
                 ("SPC / k" . vc-register)
                 ("SPC / l" . vc-print-log)
                 ("SPC / m" . vc-dir)
                 ("SPC / n" . vc-root-diff)
                 ("SPC / o" . vc-merge)
                 ("SPC / p" . vc-print-root-log)
                 ("SPC / u" . vc-push)
                 ("SPC / v" . vc-next-action)
                 ("SPC / y" . vc-revert)
                 ("SPC ;" . save-buffer)
                 ("SPC SPC 2" . jacob-sm2)
                 ("SPC SPC T" . google-translate-query-translate-reverse)
                 ("SPC SPC e a" . eglot-code-actions)
                 ("SPC SPC e e" . eglot)
                 ("SPC SPC e h" . eglot-inlay-hints-mode)
                 ("SPC SPC e i" . eglot-find-implementation)
                 ("SPC SPC e o" . eglot-code-action-organize-imports)
                 ("SPC SPC e r" . eglot-rename)
                 ("SPC SPC e t" . eglot-find-typeDefinition)
                 ("SPC SPC o a" . org-agenda)
                 ("SPC SPC o c" . org-capture)
                 ("SPC SPC o l" . org-store-link)
                 ("SPC SPC t" . google-translate-query-translate)
                 ("SPC SPC y i" . yas-insert-snippet)
                 ("SPC SPC y n" . yas-new-snippet)
                 ("SPC SPC y v" . yas-visit-snippet-file)
                 ("SPC a" . mark-whole-buffer)
                 ("SPC b" . xah-toggle-previous-letter-case)
                 ("SPC c" . jacob-copy-buffer)
                 ("SPC e ." . isearch-forward-word)
                 ("SPC e d" . highlight-regexp)
                 ("SPC e e" . highlight-symbol-at-point)
                 ("SPC e g" . isearch-forward-symbol)
                 ("SPC e j" . highlight-lines-matching-regexp)
                 ("SPC e u" . unhighlight-regexp)
                 ("SPC e y" . highlight-phrase)
                 ("SPC f" . consult-buffer)
                 ("SPC g h" . kill-paragraph)
                 ("SPC g j" . mark-paragraph)
                 ("SPC h" . beginning-of-buffer)
                 ("SPC i ," . xah-open-in-external-app)
                 ("SPC i ;" . write-file)
                 ("SPC i d" . ibuffer)
                 ("SPC i e" . find-file)
                 ("SPC i f" . ffap)
                 ("SPC i j" . consult-recent-file)
                 ("SPC i o" . consult-bookmark)
                 ("SPC i s" . xah-show-in-desktop)
                 ("SPC j b" . describe-command)
                 ("SPC j c" . consult-man)
                 ("SPC j c" . man)
                 ("SPC j g" . consult-info)
                 ("SPC j i" . describe-char)
                 ("SPC j j" . consult-symbol)
                 ("SPC j k" . describe-function)
                 ("SPC j l" . describe-variable)
                 ("SPC j m" . describe-mode)
                 ("SPC j v" . describe-key)
                 ("SPC j y" . describe-face)
                 ("SPC k ," . consult-imenu)
                 ("SPC k b" . jacob-title-case)
                 ("SPC k c" . consult-register-store)
                 ("SPC k d" . consult-line)
                 ("SPC k e" . sort-lines)
                 ("SPC k f" . kill-matching-lines)
                 ("SPC k g" . delete-non-matching-lines)
                 ("SPC k h" . mark-defun)
                 ("SPC k i" . copy-matching-lines)
                 ("SPC k j" . repeat-complex-command)
                 ("SPC k k" . repeat)
                 ("SPC k r" . query-replace-regexp)
                 ("SPC k t" . delete-duplicate-lines)
                 ("SPC k u" . consult-goto-line)
                 ("SPC k v" . consult-register-load)
                 ("SPC k w" . sort-numeric-fields)
                 ("SPC k y" . move-to-column)
                 ("SPC l ," . eww)
                 ("SPC l 6" . calendar)
                 ("SPC l ;" . jacob-screen-sharing-mode)
                 ("SPC l a" . global-text-scale-adjust)
                 ("SPC l d" . eshell)
                 ("SPC l e" . toggle-frame-maximized)
                 ("SPC l f" . shell)
                 ("SPC l g" . make-frame-command)
                 ("SPC l i" . toggle-case-fold-search)
                 ("SPC l j" . widen)
                 ("SPC l k" . narrow-to-defun)
                 ("SPC l l" . narrow-to-region)
                 ("SPC l n" . toggle-debug-on-error)
                 ("SPC l o" . count-words)
                 ("SPC m" . dired-jump)
                 ("SPC n" . end-of-buffer)
                 ("SPC o c" . copy-rectangle-as-kill)
                 ("SPC o d" . kmacro-call-macro)
                 ("SPC o e" . kmacro-start-macro)
                 ("SPC o h" . delete-rectangle)
                 ("SPC o i" . string-rectangle)
                 ("SPC o l" . rectangle-number-lines)
                 ("SPC o o" . rectangle-mark-mode)
                 ("SPC o p" . clear-rectangle)
                 ("SPC o r" . kmacro-end-macro)
                 ("SPC o s" . open-rectangle)
                 ("SPC o v" . yank-rectangle)
                 ("SPC o x" . kill-rectangle)
                 ("SPC p" . ,project-prefix-map)
                 ("SPC r" . query-replace)
                 ("SPC s" . exchange-point-and-mark)
                 ("SPC u" . kill-current-buffer)
                 ("SPC v" . consult-yank-from-kill-ring)
                 ("SPC w j" . xref-find-references)
                 ("SPC w k" . xref-find-definitions)
                 ("SPC w l" . xref-go-back)
                 ("SPC y" . isearch-forward-symbol-at-point)
                 ("\\" . embark-act)
                 ("`" . other-frame)
                 ("a" . execute-extended-command)
                 ("b" . xah-toggle-letter-case)
                 ("c" . jacob-copy-line-or-region)
                 ("d" . jacob-delete-backwards)
                 ("e" . puni-backward-kill-word)
                 ("f" . jme-command-mode-deactivate)
                 ("g" . jacob-mark)
                 ("h" . jacob-beginning-of-line)
                 ("i" . previous-line)
                 ("j" . backward-char)
                 ("k" . next-line)
                 ("l" . forward-char)
                 ("m" . puni-backward-sexp-or-up-list)
                 ("n" . isearch-forward)
                 ("o" . forward-word)
                 ("p" . recenter-top-bottom)
                 ("q" . duplicate-dwim)
                 ("r" . puni-forward-kill-word)
                 ("s" . jacob-return-macro)
                 ("t" . puni-slurp-forward) ; experimental, is this too lisp specific?
                 ("u" . backward-word)
                 ("v" . yank)
                 ("w" . jacob-shrink-whitespaces)
                 ("x" . jacob-kill-line)
                 ("y" . undo)
                 ("z" . xah-comment-dwim)
                 ("'" . delete-other-windows)))

;; ("c f" . xah-open-recently-closed)
;; ("c g" . xah-open-in-terminal)
;; ("c j" . xah-copy-file-path)
;; ("c n" . xah-new-empty-buffer)
;; ("c p" . xah-open-last-closed)
;; ("c x" . set-buffer-file-coding-system)
;; ("c y" . xah-list-recently-closed)
;; ("c z" . revert-buffer-with-coding-system)

(jme-major-mode-override-keys 'dired-mode
                              '(("s" . dired-find-file)
                                ("d" . dired-do-delete)
                                ("q" . quit-window)
                                ("i" . dired-previous-line)
                                ("k" . dired-next-line)
                                ("e" . dired-mark)
                                ("r" . dired-unmark)
                                ("g" . revert-buffer)
                                ("x" . dired-do-rename)
                                ("c" . dired-do-copy)
                                ("u" . dired-up-directory)
                                ("j" . dired-goto-file)
                                ("<remap> <previous-line>" . nil)
                                ("<remap> <next-line>" . nil)))

(jme-major-mode-override-keys 'minibuffer-mode
                              '(("i" . minibuffer-previous-completion)
                                ("k" . minibuffer-next-completion)))

(jme-major-mode-override-keys 'package-menu-mode
                              '(("d" . package-menu-mark-delete)
                                ("e" . package-menu-mark-install)
                                ("q" . quit-window)
                                ("r" . package-menu-mark-unmark)
                                ("s" . package-menu-describe-package)
                                ("x" . package-menu-execute)))

(jme-major-mode-override-keys 'org-mode
                              '(("SPC k b" . jacob-org-title-case ; TODO: write this command?
                                 )))

(jme-major-mode-override-keys 'org-agenda-mode
                              '(("q" . quit-window)
                                ("g" . org-agenda-redo-all)
                                ("SPC ;" . org-save-all-org-buffers)))

(jme-major-mode-override-keys 'help-mode
                              '(("w" . jacob-help-edit)
                                ("s" . help-view-source)
                                ("q" . quit-window)
                                ("e" . help-go-back)
                                ("r" . help-go-forward)
                                ("g" . revert-buffer)))

(jme-major-mode-override-keys 'Info-mode
                              '(("q" . quit-window)
                                ("r" . Info-scroll-up)
                                ("e" . Info-scroll-down)
                                ("w" . Info-up)
                                ("g" . Info-menu)))

(jme-major-mode-override-keys 'Man-mode
                              '(("q" . quit-window)
                                ("r" . scroll-up-command)
                                ("e" . scroll-down-command)
                                ("g" . Man-goto-section)))

(jme-major-mode-override-keys 'eww-mode
                              '(("q" . quit-window)
                                ("r" . scroll-up-command)
                                ("e" . scroll-down-command)))

(jme-major-mode-override-keys 'occur-mode
                              '(("q" . quit-window)
                                ("i" . occur-prev)
                                ("k" . occur-next)))

(jme-major-mode-override-keys 'diff-mode
                              '(("d" . diff-hunk-kill)
                                ("e" . diff-hunk-prev)
                                ("g" . revert-buffer)
                                ("q" . quit-window)
                                ("r" . diff-hunk-next)
                                ("v" . vc-next-action)
                                ("x" . diff-split-hunk)
                                ("y" . jacob-diff-unapply-hunk)))

(jme-major-mode-override-keys 'vc-git-log-view-mode
                              '(("q" . quit-window)))

(jme-major-mode-override-keys 'vc-dir-mode
                              '(("d" . vc-diff)
                                ("e" . vc-dir-mark)
                                ("g" . revert-buffer)
                                ("i" . vc-dir-previous-line)
                                ("k" . vc-dir-next-line)
                                ("o" . vc-dir-next-directory)
                                ("p" . vc-push)
                                ("q" . quit-window)
                                ("r" . vc-dir-unmark)
                                ("s" . vc-dir-find-file)
                                ("u" . vc-dir-previous-directory)
                                ("v" . vc-next-action)
                                ("x" . vc-dir-hide-up-to-date)))

(jme-major-mode-override-keys 'vc-annotate-mode
                              '(("q" . quit-window)
                                ("g" . revert-buffer)))

(jme-major-mode-override-keys 'prodigy-mode
                              '(("d" . prodigy-stop)
                                ("e" . prodigy-mark)
                                ("g" . consult-git-grep)
                                ("i" . prodigy-prev)
                                ("k" . prodigy-next)
                                ("q" . quit-window)
                                ("r" . prodigy-unmark)
                                ("s" . prodigy-restart)
                                ("v" . prodigy-display-process)))

(jme-major-mode-override-keys 'geiser-mode
                              '(("SPC , m" . geiser-eval-last-sexp)
                                ("SPC , d" . geiser-eval-definition)))

(jme-major-mode-override-keys 'calendar-mode
                              '(("q" . quit-window)
                                ("i" . calendar-backward-week)
                                ("k" . calendar-forward-week)
                                ("j" . calendar-backward-day)
                                ("l" . calendar-forward-day)
                                ("u" . calendar-backward-month)
                                ("o" . calendar-forward-month)
                                ("d" . diary-view-entries)
                                ("s" . diary-insert-entry)
                                ("m" . diary-mark-entries)
                                ("." . calendar-goto-today)
                                ("t" . calendar-set-mark)))

(jme-major-mode-override-keys 'compilation-mode
                              '(("g" . recompile)
                                ("q" . quit-window)))

(jme-major-mode-override-keys 'doc-view-mode
                              '(("l" . doc-view-next-page)
                                ("j" . doc-view-previous-page)))

(jme-major-mode-override-keys 'nov-mode
                              '(("e" . nov-scroll-down)
                                ("r" . nov-scroll-up)))

(jme-major-mode-override-keys 'embark-collect-mode
                              '(("q" . quit-window)))

(jme-major-mode-override-keys 'verb-response-body-mode
                              '(("q" . quit-window)))

(jme-major-mode-override-keys 'sly-mode
                              '(("SPC , m" . sly-eval-last-expression)
                                ("SPC , d" . sly-eval-defun)
                                ("SPC , e" . sly-eval-buffer)
                                ("SPC w k" . sly-edit-definition)))

(jme-major-mode-override-keys 'sly-db-mode
                              '(("q" . sly-db-quit)))

(jme-major-mode-override-keys 'pdf-view-mode
                              '(("c" . pdf-view-kill-ring-save)))

(jme-major-mode-override-keys 'ibuffer-mode
                              '(("q" . quit-window)
                                ("e" . ibuffer-mark-forward)
                                ("r" . ibuffer-unmark-forward)
                                ("g" . ibuffer-update)))

(jme-major-mode-override-keys 'sql-interactive-mode
                              '(("SPC , d" . sql-send-paragraph)))

(defun jme--wgrep-handle-w ()
  "Handle the \"w\" key being pressed in grep-mode."
  (interactive)
  (jme--wgrep--helper "w" #'wgrep-change-to-wgrep-mode))

(defun jme--wgrep-handle-e ()
  "Handle the \"e\" key being pressed in grep-mode."
  (interactive)
  (jme--wgrep--helper "e" #'previous-error-no-select))

(defun jme--wgrep-handle-r ()
  "Handle the \"r\" key being pressed in grep-mode."
  (interactive)
  (jme--wgrep--helper "r" #'next-error-no-select))

(defun jme--wgrep--helper (key command)
  "Helper for `jacob-modal-editing' in grep/wgrep modes.

- If wgrep is active, use the command bound to KEY in
  `jacob-modal-editing-command-mode-map'.
- If wgrep is inactive, use COMMAND."
  (call-interactively (if (equal (current-local-map) wgrep-mode-map)
                          (keymap-lookup jacob-modal-editing-command-mode-map
                                         key)
                        command)))

(jme-major-mode-override-keys 'grep-mode
                              '(("w" . jme--wgrep-handle-w)
                                ("e" . jme--wgrep-handle-e)
                                ("r" . jme--wgrep-handle-r)))

(jme-major-mode-override-keys 'gnus-group-mode
                              '(("q" . gnus-group-quit)))

(jme-major-mode-override-keys 'magit-status-mode
                              '(("." . magit-section-forward)
                                ("e" . magit-stage)
                                ("m" . magit-section-backward)
                                ("q" . magit-mode-bury-buffer)
                                ("r" . magit-unstage)
                                ("y" . magit-discard)))

(defun jme-command-mode-hook-f ()
  "Make visual change depending on value of `jacob-modal-editing-command-mode'."
  (unless (equal jme-command-mode global-hl-line-mode)
    (global-hl-line-mode (if jme-command-mode 1 -1)))
  (modify-all-frames-parameters `((cursor-type . ,(if jme-command-mode 'box 'bar)))))

(add-hook 'jme-command-mode-hook #'jme-command-mode-hook-f)

(keymap-set jme-mode-map "M-SPC" #'jme-command-mode-activate)

(defun jme-mode-hook-function ()
  "Hook function for `jacob-modal-editing-mode'."
  (if jme-mode
      (progn
        (add-hook 'minibuffer-setup-hook #'jme-command-mode-deactivate)
        (add-hook 'minibuffer-exit-hook #'jme-command-mode-activate))
    (remove-hook 'minibuffer-setup-hook #'jme-command-mode-deactivate)
    (remove-hook 'minibuffer-exit-hook #'jme-command-mode-activate)))

(add-hook 'jme-mode-hook #'jme-mode-hook-function)

;; eat.el integration

(defun jme-setup-eat ()
  "Configure eat for modal editing."
  (when eat--eshell-process-running-mode
    (if jme-command-mode
        (eat-eshell-emacs-mode)
      (eat-eshell-semi-char-mode))))

(with-eval-after-load "eat"
  (add-hook 'jme-command-mode-hook #'jme-setup-eat)
  (add-hook 'eat-eshell-exec-hook #'jme-setup-eat))

(provide 'jme-config)

;;; jacob-modal-editing-config.el ends here

;; Local Variables:
;; read-symbol-shorthands: (("jme-" . "jacob-modal-editing-"))
;; End:
