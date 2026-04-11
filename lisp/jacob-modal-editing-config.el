;;; jacob-modal-editing-config.el --- configuration for `jacob-modal-editing'

;;; Commentary:
;; 

;;; Code:

(defun jacob-modal-bind-keys (map bindings)
  "Bind multiple keys in MAP. BINDINGS is an alist of (key . command)."
  (dolist (binding bindings)
    (keymap-set map (car binding) (cdr binding))))

(jacob-modal-bind-keys jacob-modal-editing-keymap
                       `(("f" . jacob-modal-editing-disable)
                         ("j" . backward-char)
                         ("k" . next-line)
                         ("i" . previous-line)
                         ("l" . forward-char)
                         ("u" . backward-word)
                         ("o" . forward-word)
                         ("h" . jacob-beginning-of-line)
                         (";" . jacob-end-of-line)
                         ("m" . puni-backward-sexp-or-up-list)
                         ("." . puni-forward-sexp-or-up-list)
                         ("," . jacob-split-or-switch-window)
                         ("`" . other-frame)
                         ("y" . undo)
                         ("n" . isearch-forward)
                         ("a" . execute-extended-command)
                         ("s" . jacob-return-macro)
                         ("d" . jacob-delete-backwards)
                         ("w" . jacob-shrink-whitespaces)
                         ("e" . puni-backward-kill-word)
                         ("r" . puni-forward-kill-word)
                         ("t" . set-mark-command)
                         ("p" . recenter-top-bottom)
                         ("x" . jacob-kill-line)
                         ("c" . jacob-copy-line-or-region)
                         ("v" . yank)
                         ("z" . xah-comment-dwim)
                         ("b" . xah-toggle-letter-case)
                         ("g" . expreg-expand)
                         ("\\" . embark-act)
                         ("q" . jacob-format-words)
                         ("'" . delete-other-windows)
                         ("@" . delete-ndow)
                         ("-" . split-window-below)
                         ("=" . split-window-right)
                         ("1" . winner-undo)
                         ("2" . winner-redo)
                         ("4" . other-window-prefix)
                         ("5" . delete-forward-char)
                         ("6" . jacob-mark-paragraph)
                         ("7" . jacob-mark-line)
                         ("0" . pop-to-mark-command)
                         ("SPC '" . delete-window)
                         ("SPC ." . universal-argument)
                         ("SPC ;" . save-buffer)
                         ("SPC a" . mark-whole-buffer)
                         ("SPC b" . xah-toggle-previous-letter-case)
                         ("SPC c" . jacob-copy-buffer)
                         ("SPC f" . consult-buffer)
                         ("SPC h" . beginning-of-buffer)
                         ("SPC m" . dired-jump)
                         ("SPC n" . end-of-buffer)
                         ("SPC p" . ,project-prefix-map)
                         ("SPC p f" . consult-project-extra-find)
                         ("SPC r" . query-replace)
                         ("SPC s" . exchange-point-and-mark)
                         ("SPC u" . kill-current-buffer)
                         ("SPC v" . consult-yank-from-kill-ring)
                         ("SPC y" . isearch-forward-symbol-at-point)
                         ("SPC e ." . isearch-forward-word)
                         ("SPC e d" . highlight-regexp)
                         ("SPC e e" . highlight-symbol-at-point)
                         ("SPC e g" . isearch-forward-symbol)
                         ("SPC e j" . highlight-lines-matching-regexp)
                         ("SPC e s" . consult-line)
                         ("SPC e u" . unhighlight-regexp)
                         ("SPC e y" . highlight-phrase)
                         ("SPC , ," . delete-frame)
                         ("SPC , d" . eval-defun)
                         ("SPC , e" . eval-buffer)
                         ("SPC , f" . eval-region)
                         ("SPC , m" . eval-last-sexp)
                         ("SPC , n" . jacob-eval-and-replace)
                         ("SPC , r" . eval-expression)
                         ("SPC , x" . save-buffers-kill-emacs)
                         ("SPC j c" . man)
                         ("SPC j y" . describe-face)
                         ("SPC j i" . describe-char)
                         ("SPC j g" . info)
                         ("SPC j m" . describe-mode)
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
                         ("SPC g h" . kill-paragraph)
                         ("SPC g j" . mark-paragraph)
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
                         ("SPC i ," . xah-open-in-external-app)
                         ("SPC i ;" . write-file)
                         ("SPC i d" . ibuffer)
                         ("SPC i e" . find-file)
                         ("SPC i f" . ffap)
                         ("SPC i j" . consult-recent-file)
                         ("SPC i o" . consult-bookmark)
                         ("SPC i s" . xah-show-in-desktop)
                         ("SPC k c" . consult-register-store)
                         ("SPC k i" . consult-register-load)
                         ("SPC k k" . repeat)
                         ("SPC k r" . query-replace-regexp)
                         ("SPC k u" . consult-goto-line)
                         ("SPC w j" . xref-find-references)
                         ("SPC w k" . xref-find-definitions)
                         ("SPC w l" . xref-go-back)
                         ("SPC / j" . vc-diff)
                         ("SPC / h" . vc-annotate)
                         ("SPC / m" . magit-project-status)
                         ("SPC j b" . describe-command)
                         ("SPC j c" . consult-man)
                         ("SPC j g" . consult-info)
                         ("SPC j j" . consult-symbol)
                         ("SPC j k" . describe-function)
                         ("SPC j l" . describe-variable)
                         ("SPC j v" . describe-key)
                         ("SPC SPC e e" . eglot)
                         ("SPC SPC e a" . eglot-code-actions)
                         ("SPC SPC e i" . eglot-find-implementation)
                         ("SPC SPC e r" . eglot-rename)
                         ("SPC SPC e t" . eglot-find-typeDefinition)
                         ("SPC SPC e h" . eglot-inlay-hints-mode)
                         ("SPC SPC e o" . eglot-code-action-organize-imports)
                         ("SPC SPC a a" . org-agenda)
                         ("SPC SPC a c" . org-capture)))

;; ("c f" . xah-open-recently-closed)
;; ("c g" . xah-open-in-terminal)
;; ("c j" . xah-copy-file-path)
;; ("c n" . xah-new-empty-buffer)
;; ("c p" . xah-open-last-closed)
;; ("c x" . set-buffer-file-coding-system)
;; ("c y" . xah-list-recently-closed)
;; ("c z" . revert-buffer-with-coding-system)

(defvar-keymap jacob-modal-editing-dired-map
  "s" #'dired-find-file
  "d" #'dired-do-delete
  "q" #'quit-window
  "i" #'dired-previous-line
  "k" #'dired-next-line
  "e" #'dired-mark
  "r" #'dired-unmark
  "g" #'revert-buffer
  "x" #'dired-do-rename
  "c" #'dired-do-copy
  "u" #'dired-up-directory
  "j" #'dired-goto-file
  "<remap> <previous-line>" nil
  "<remap> <next-line>" nil)

(add-to-list 'jacob-modal-editing-major-mode-keymap-alist `(dired-mode . ,jacob-modal-editing-dired-map))

(defvar-keymap jacob-modal-editing-package-menu-map
  "d" #'package-menu-mark-delete
  "e" #'package-menu-mark-install
  "q" #'quit-window
  "r" #'package-menu-mark-unmark
  "s" #'package-menu-describe-package
  "x" #'package-menu-execute)

(add-to-list 'jacob-modal-editing-major-mode-keymap-alist `(package-menu-mode . ,jacob-modal-editing-package-menu-map))

(defvar-keymap jacob-modal-editing-org-agenda-map
  "q" #'quit-window
  "g" #'org-agenda-redo-all)

(add-to-list 'jacob-modal-editing-major-mode-keymap-alist
             `(org-agenda-mode . ,jacob-modal-editing-org-agenda-map))

(defvar-keymap jacob-modal-editing-help-map
  "w" #'jacob-help-edit
  "s" #'help-view-source
  "q" #'quit-window
  "e" #'help-go-back
  "r" #'help-go-forward
  "g" #'revert-buffer)

(add-to-list 'jacob-modal-editing-major-mode-keymap-alist
             `(help-mode . ,jacob-modal-editing-help-map))

(defvar-keymap jacob-modal-editing-info-map
  "q" #'quit-window
  "r" #'Info-scroll-up
  "e" #'Info-scroll-down
  "w" #'Info-up
  "g" #'Info-menu)

(add-to-list 'jacob-modal-editing-major-mode-keymap-alist
             `(Info-mode . ,jacob-modal-editing-info-map))

(defvar-keymap jacob-modal-editing-man-map
  "q" #'quit-window)

(add-to-list 'jacob-modal-editing-major-mode-keymap-alist
             `(Man-mode . ,jacob-modal-editing-man-map))

(defvar-keymap jacob-modal-editing-occur-map
  "q" #'quit-window
  "i" #'occur-prev
  "k" #'occur-next)

(add-to-list 'jacob-modal-editing-major-mode-keymap-alist
             `(occur-mode . ,jacob-modal-editing-occur-map))

(defvar-keymap jacob-modal-editing-diff-map
  "q" #'quit-window
  "e" #'diff-hunk-prev
  "r" #'diff-hunk-next
  "x" #'diff-hunk-kill
  "g" #'revert-buffer)

(add-to-list 'jacob-modal-editing-major-mode-keymap-alist
             `(diff-mode . ,jacob-modal-editing-diff-map))

(defvar-keymap jacob-modal-editing-vc-git-log-map
  "q" #'quit-window)

(add-to-list 'jacob-modal-editing-major-mode-keymap-alist
             `(vc-git-log-view-mode . ,jacob-modal-editing-vc-git-log-map))

(defvar-keymap jacob-modal-editing-vc-dir-map
  "q" #'quit-window
  "g" #'revert-buffer
  "i" #'vc-dir-previous-line
  "k" #'vc-dir-next-line
  "o" #'vc-dir-next-directory
  "u" #'vc-dir-previous-directory
  "s" #'vc-dir-find-file
  "e" #'vc-dir-mark
  "r" #'vc-dir-unmark
  "v" #'vc-next-action
  "p" #'vc-push
  "=" #'vc-diff
  "x" #'vc-dir-hide-up-to-date)

(add-to-list 'jacob-modal-editing-major-mode-keymap-alist
             `(vc-dir-mode . ,jacob-modal-editing-vc-dir-map))

(defvar-keymap jacob-modal-editing-vc-annotate-map
  "q" #'quit-window
  "g" #'revert-buffer)

(add-to-list 'jacob-modal-editing-major-mode-keymap-alist
             `(vc-annotate-mode . ,jacob-modal-editing-vc-annotate-map))

(defvar-keymap jacob-modal-editing-prodigy-map
  "d" #'prodigy-stop
  "e" #'prodigy-mark
  "g" #'consult-git-grep
  "f" #'project-find-file
  "i" #'prodigy-prev
  "k" #'prodigy-next
  "q" #'quit-window
  "r" #'prodigy-unmark
  "s" #'prodigy-restart
  "v" #'prodigy-display-process)

(add-to-list 'jacob-modal-editing-major-mode-keymap-alist
             `(prodigy-mode . ,jacob-modal-editing-prodigy-map))

(defvar-keymap jacob-modal-editing-geiser-map
  "SPC , m" #'geiser-eval-last-sexp
  "SPC , d" #'geiser-eval-definition)

(add-to-list 'jacob-modal-editing-major-mode-keymap-alist
             `(geiser-mode . ,jacob-modal-editing-geiser-map))

(defvar-keymap jacob-modal-editing-calendar-map
  "q" #'quit-window
  "i" #'calendar-backward-week
  "k" #'calendar-forward-week
  "j" #'calendar-backward-day
  "l" #'calendar-forward-day
  "u" #'calendar-backward-month
  "o" #'calendar-forward-month
  "d" #'diary-view-entries
  "s" #'diary-insert-entry
  "m" #'diary-mark-entries
  "." #'calendar-goto-today
  "t" #'calendar-set-mark)

(add-to-list 'jacob-modal-editing-major-mode-keymap-alist
             `(calendar-mode . ,jacob-modal-editing-calendar-map))

(defvar-keymap jacob-modal-editing-compilation-map
  "g" #'recompile
  "q" #'quit-window)

(add-to-list 'jacob-modal-editing-major-mode-keymap-alist
             `(compilation-mode . ,jacob-modal-editing-compilation-map))

(defvar-keymap jacob-modal-editing-doc-view-map
  "l" #'doc-view-next-page
  "j" #'doc-view-previous-page)

(add-to-list 'jacob-modal-editing-major-mode-keymap-alist
             `(doc-view-mode . ,jacob-modal-editing-doc-view-map))

(defvar-keymap jacob-modal-editing-embark-collect-map
  "q" #'quit-window)

(add-to-list 'jacob-modal-editing-major-mode-keymap-alist
             `(embark-collect-mode . ,jacob-modal-editing-embark-collect-map))

(defvar-keymap jacob-modal-editing-verb-response-map
  "q" #'quit-window)

(add-to-list 'jacob-modal-editing-major-mode-keymap-alist
             `(verb-response-body-mode . ,jacob-modal-editing-verb-response-map))

(defvar-keymap jacob-modal-editing-sly-map
  "SPC , m" #'sly-eval-last-expression
  "SPC , d" #'sly-eval-defun
  "SPC , e" #'sly-eval-buffer
  "SPC w k" #'sly-edit-definition)

(add-to-list 'jacob-modal-editing-major-mode-keymap-alist
             `(sly-mode . ,jacob-modal-editing-sly-map))

(defvar-keymap jacob-modal-editing-sly-db-map
  "q" #'sly-db-quit)

(add-to-list 'jacob-modal-editing-major-mode-keymap-alist
             `(sly-db-mode . ,jacob-modal-editing-sly-db-map))

(defvar-keymap jacob-modal-editing-pdf-view-map
  "c" #'pdf-view-kill-ring-save)

(add-to-list 'jacob-modal-editing-major-mode-keymap-alist
             `(pdf-view-mode . ,jacob-modal-editing-pdf-view-map))

(defvar-keymap jacob-modal-editing-ibuffer-map
  "q" #'quit-window
  "e" #'ibuffer-mark-forward
  "r" #'ibuffer-unmark-forward
  "g" #'ibuffer-update)

(add-to-list 'jacob-modal-editing-major-mode-keymap-alist
             `(ibuffer-mode . ,jacob-modal-editing-ibuffer-map))

(defvar-keymap jacob-modal-editing-sql-interactive-map
  "SPC , d" #'sql-send-paragraph)

(add-to-list 'jacob-modal-editing-major-mode-keymap-alist
             `(sql-interactive-mode . ,jacob-modal-editing-sql-interactive-map))

(defvar-keymap jacob-modal-editing-grep-interactive-map
  "w" #'wgrep-change-to-wgrep-mode
  "e" #'previous-error-no-select
  "r" #'next-error-no-select)

(add-to-list 'jacob-modal-editing-major-mode-keymap-alist
             `(grep-mode . ,jacob-modal-editing-grep-interactive-map))

(defun jacob-modal-editing-command-mode-hook-f ()
  "Make visual change depending on value of `jacob-modal-editing-command-mode'."
  (unless (equal jacob-modal-editing-command-mode global-hl-line-mode)
    (global-hl-line-mode (if jacob-modal-editing-command-mode 1 -1)))
  (modify-all-frames-parameters `((cursor-type . ,(if jacob-modal-editing-command-mode 'box 'bar)))))

(add-hook 'jacob-modal-editing-command-mode-hook #'jacob-modal-editing-command-mode-hook-f)

;; eat.el integration

(defun jacob-modal-editing-setup-eat ()
  "Configure eat for modal editing."
  (when eat--eshell-process-running-mode
    (if jacob-modal-editing-command-mode
        (eat-eshell-emacs-mode)
      (eat-eshell-semi-char-mode))))

(with-eval-after-load "eat"
  (add-hook 'jacob-modal-editing-command-mode-hook #'jacob-modal-editing-setup-eat)
  (add-hook 'eat-eshell-exec-hook #'jacob-modal-editing-setup-eat))

(keymap-set jacob-modal-editing-mode-keymap "M-SPC" #'jacob-modal-editing-enable)

(defun jacob-modal-editing-mode-hook-function ()
  "Hook function for `jacob-modal-editing-mode'."
  (if jacob-modal-editing-mode
      (progn
        (add-hook 'minibuffer-setup-hook #'jacob-modal-editing-disable)
        (add-hook 'minibuffer-exit-hook #'jacob-modal-editing-enable))
    (remove-hook 'minibuffer-setup-hook #'jacob-modal-editing-disable)
    (remove-hook 'minibuffer-exit-hook #'jacob-modal-editing-enable)))

(add-hook 'jacob-modal-editing-mode-hook #'jacob-modal-editing-mode-hook-function)

(provide 'jacob-modal-editing-config)

;;; jacob-modal-editing-config.el ends here
