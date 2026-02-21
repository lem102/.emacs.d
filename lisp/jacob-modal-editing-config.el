;;; jacob-modal-editing-config.el --- configuration for `jacob-modal-editing'


;;; Commentary:
;; 

;;; Code:

(keymap-set jacob-modal-editing-keymap "f" #'jacob-modal-editing-disable)

;; Navigation
(keymap-set jacob-modal-editing-keymap "j" #'backward-char)
(keymap-set jacob-modal-editing-keymap "k" #'next-line)
(keymap-set jacob-modal-editing-keymap "i" #'previous-line)
(keymap-set jacob-modal-editing-keymap "l" #'forward-char)
(keymap-set jacob-modal-editing-keymap "u" #'backward-word)
(keymap-set jacob-modal-editing-keymap "o" #'forward-word)
(keymap-set jacob-modal-editing-keymap "h" #'jacob-beginning-of-line)
(keymap-set jacob-modal-editing-keymap ";" #'jacob-end-of-line)
(keymap-set jacob-modal-editing-keymap "m" #'puni-backward-sexp-or-up-list)
(keymap-set jacob-modal-editing-keymap "." #'puni-forward-sexp-or-up-list)

;; Editing & Actions
(keymap-set jacob-modal-editing-keymap "y" #'undo)
(keymap-set jacob-modal-editing-keymap "n" #'isearch-forward)
(keymap-set jacob-modal-editing-keymap "," #'other-window)
(keymap-set jacob-modal-editing-keymap "a" #'execute-extended-command)
(keymap-set jacob-modal-editing-keymap "s" #'jacob-return-macro)
(keymap-set jacob-modal-editing-keymap "d" #'jacob-backspace)
(keymap-set jacob-modal-editing-keymap "w" #'xah-shrink-whitespaces)
(keymap-set jacob-modal-editing-keymap "e" #'backward-kill-word)
(keymap-set jacob-modal-editing-keymap "r" #'kill-word)
(keymap-set jacob-modal-editing-keymap "t" #'set-mark-command)
(keymap-set jacob-modal-editing-keymap "p" #'recenter-top-bottom)
(keymap-set jacob-modal-editing-keymap "x" #'jacob-kill-line)
(keymap-set jacob-modal-editing-keymap "c" #'jacob-copy-line-or-region)
(keymap-set jacob-modal-editing-keymap "v" #'yank)
(keymap-set jacob-modal-editing-keymap "z" #'xah-comment-dwim)
(keymap-set jacob-modal-editing-keymap "b" #'xah-toggle-letter-case)
(keymap-set jacob-modal-editing-keymap "g" #'expreg-expand)
(keymap-set jacob-modal-editing-keymap "\\" #'embark-act)

;; Windows & UI
(keymap-set jacob-modal-editing-keymap "'" #'delete-other-windows)
(keymap-set jacob-modal-editing-keymap "@" #'delete-window)
(keymap-set jacob-modal-editing-keymap "-" #'split-window-below)
(keymap-set jacob-modal-editing-keymap "=" #'split-window-right)

;; Numbers & Marks
(keymap-set jacob-modal-editing-keymap "1" #'winner-undo)
(keymap-set jacob-modal-editing-keymap "2" #'winner-redo)
(keymap-set jacob-modal-editing-keymap "4" #'other-window-prefix)
(keymap-set jacob-modal-editing-keymap "5" #'delete-forward-char)
(keymap-set jacob-modal-editing-keymap "6" #'jacob-mark-paragraph)
(keymap-set jacob-modal-editing-keymap "7" #'jacob-mark-line)
(keymap-set jacob-modal-editing-keymap "0" #'pop-to-mark-command)

;; Top-level SPC commands
(keymap-set jacob-modal-editing-keymap "SPC '" #'delete-window)
(keymap-set jacob-modal-editing-keymap "SPC ;" #'save-buffer)
(keymap-set jacob-modal-editing-keymap "SPC ." #'universal-argument)
(keymap-set jacob-modal-editing-keymap "SPC a" #'mark-whole-buffer)
(keymap-set jacob-modal-editing-keymap "SPC b" #'xah-toggle-previous-letter-case)
(keymap-set jacob-modal-editing-keymap "SPC c" #'jacob-copy-buffer)
(keymap-set jacob-modal-editing-keymap "SPC m" #'dired-jump)
(keymap-set jacob-modal-editing-keymap "SPC n" #'end-of-buffer)
(keymap-set jacob-modal-editing-keymap "SPC h" #'beginning-of-buffer)
(keymap-set jacob-modal-editing-keymap "SPC p" project-prefix-map)
(keymap-set jacob-modal-editing-keymap "SPC y" #'isearch-forward-symbol-at-point)
(keymap-set jacob-modal-editing-keymap "SPC s" #'exchange-point-and-mark)
(keymap-set jacob-modal-editing-keymap "SPC u" #'kill-current-buffer)
(keymap-set jacob-modal-editing-keymap "SPC r" #'visual-replace)

;; Nested: SPC e (Highlighting)
(keymap-set jacob-modal-editing-keymap "SPC e e" #'highlight-symbol-at-point)
(keymap-set jacob-modal-editing-keymap "SPC e u" #'unhighlight-regexp)

;; Nested: SPC , (Evaluation/System)
(keymap-set jacob-modal-editing-keymap "SPC , ," #'delete-frame)
(keymap-set jacob-modal-editing-keymap "SPC , d" #'eval-defun)
(keymap-set jacob-modal-editing-keymap "SPC , e" #'eval-buffer)
(keymap-set jacob-modal-editing-keymap "SPC , f" #'eval-region)
(keymap-set jacob-modal-editing-keymap "SPC , m" #'eval-last-sexp)
(keymap-set jacob-modal-editing-keymap "SPC , r" #'eval-expression)
(keymap-set jacob-modal-editing-keymap "SPC , x" #'save-buffers-kill-emacs)

;; Nested: SPC j (Help/Info)
(keymap-set jacob-modal-editing-keymap "SPC j c" #'man)
(keymap-set jacob-modal-editing-keymap "SPC j y" #'describe-face)
(keymap-set jacob-modal-editing-keymap "SPC j i" #'describe-char)
(keymap-set jacob-modal-editing-keymap "SPC j g" #'info)

;; Nested: SPC l (Utilities/UI)
(keymap-set jacob-modal-editing-keymap "SPC l 6" #'calendar)
(keymap-set jacob-modal-editing-keymap "SPC l a" #'global-text-scale-adjust)
(keymap-set jacob-modal-editing-keymap "SPC l ;" #'global-display-line-numbers-mode)
(keymap-set jacob-modal-editing-keymap "SPC l d" #'eshell)
(keymap-set jacob-modal-editing-keymap "SPC l e" #'toggle-frame-maximized)
(keymap-set jacob-modal-editing-keymap "SPC l f" #'shell)
(keymap-set jacob-modal-editing-keymap "SPC l g" #'make-frame-command)
(keymap-set jacob-modal-editing-keymap "SPC l o" #'count-words)
(keymap-set jacob-modal-editing-keymap "SPC l n" #'toggle-debug-on-error)
(keymap-set jacob-modal-editing-keymap "SPC l i" #'toggle-case-fold-search)

;; Nested: SPC g (Paragraphs)
(keymap-set jacob-modal-editing-keymap "SPC g h" #'kill-paragraph)
(keymap-set jacob-modal-editing-keymap "SPC g j" #'mark-paragraph)

;; Nested: SPC o (Rectangles & Macros)
(keymap-set jacob-modal-editing-keymap "SPC o d" #'kmacro-call-macro)
(keymap-set jacob-modal-editing-keymap "SPC o e" #'kmacro-start-macro)
(keymap-set jacob-modal-editing-keymap "SPC o h" #'delete-rectangle)
(keymap-set jacob-modal-editing-keymap "SPC o i" #'string-rectangle)
(keymap-set jacob-modal-editing-keymap "SPC o l" #'rectangle-number-lines)
(keymap-set jacob-modal-editing-keymap "SPC o o" #'rectangle-mark-mode)
(keymap-set jacob-modal-editing-keymap "SPC o p" #'clear-rectangle)
(keymap-set jacob-modal-editing-keymap "SPC o r" #'kmacro-end-macro)
(keymap-set jacob-modal-editing-keymap "SPC o s" #'open-rectangle)
(keymap-set jacob-modal-editing-keymap "SPC o v" #'yank-rectangle)
(keymap-set jacob-modal-editing-keymap "SPC o x" #'kill-rectangle)

;; Nested: SPC i (Files)
(keymap-set jacob-modal-editing-keymap "SPC i d" #'ibuffer)
(keymap-set jacob-modal-editing-keymap "SPC i e" #'find-file)

;; Nested: SPC k
(keymap-set jacob-modal-editing-keymap "SPC k c" #'copy-to-register)
(keymap-set jacob-modal-editing-keymap "SPC k k" #'repeat)
(keymap-set jacob-modal-editing-keymap "SPC k r" #'visual-replace-regexp)

;; Nested: SPC w (Xref)
(keymap-set jacob-modal-editing-keymap "SPC w j" #'xref-find-references)
(keymap-set jacob-modal-editing-keymap "SPC w k" #'xref-find-definitions)
(keymap-set jacob-modal-editing-keymap "SPC w l" #'xref-go-back)

;; Nested: SPC / (VC)
(keymap-set jacob-modal-editing-keymap "SPC / j" #'vc-diff)
(keymap-set jacob-modal-editing-keymap "SPC / h" #'vc-annotate)
(keymap-set jacob-modal-editing-keymap "SPC / m" #'magit-project-status)

;; SPC Consult / Search Commands
(keymap-set jacob-modal-editing-keymap "SPC v" #'consult-yank-from-kill-ring)
(keymap-set jacob-modal-editing-keymap "SPC f" #'consult-buffer)

;; Nested: SPC i (Recent files & Bookmarks)
(keymap-set jacob-modal-editing-keymap "SPC i j" #'consult-recent-file)
(keymap-set jacob-modal-editing-keymap "SPC i o" #'consult-bookmark)

;; Nested: SPC e (Search)
(keymap-set jacob-modal-editing-keymap "SPC e s" #'consult-line)

;; Nested: SPC k (Registers & Navigation)
(keymap-set jacob-modal-editing-keymap "SPC k i" #'consult-register-load)
(keymap-set jacob-modal-editing-keymap "SPC k u" #'consult-goto-line)

;; Nested: SPC j
(keymap-set jacob-modal-editing-keymap "SPC j b" #'helpful-command)
(keymap-set jacob-modal-editing-keymap "SPC j c" #'consult-man)
(keymap-set jacob-modal-editing-keymap "SPC j g" #'consult-info)
(keymap-set jacob-modal-editing-keymap "SPC j k" #'helpful-callable)
(keymap-set jacob-modal-editing-keymap "SPC j l" #'helpful-variable)
(keymap-set jacob-modal-editing-keymap "SPC j v" #'helpful-key)

;; eglot

(defvar-keymap jacob-modal-editing-eglot-map
  "e" #'eglot
  "a" #'eglot-code-actions
  "i" #'eglot-find-implementation
  "r" #'eglot-rename
  "t" #'eglot-find-typeDefinition)

(keymap-set jacob-modal-editing-keymap "SPC SPC c" (cons "Code" jacob-modal-editing-eglot-map))

;; org agenda

(defvar-keymap jacob-modal-editing-org-agenda-map
  "a" #'org-agenda
  "c" #'org-capture)

(keymap-set jacob-modal-editing-keymap "SPC SPC a" (cons "Agenda" jacob-modal-editing-org-agenda-map))

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

(defvar-keymap jacob-modal-editing-helpful-map
  "q" #'quit-window
  "g" #'helpful-update
  "e" #'backward-button
  "r" #'forward-button
  "s" #'push-button)

(add-to-list 'jacob-modal-editing-major-mode-keymap-alist
             `(helpful-mode . ,jacob-modal-editing-helpful-map))

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
  "g" #'jacob-project-search ; FIXME: command not found sometimes
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

(defun jacob-modal-editing-hook-f (command-state-p)
  "Make visual change depending on value of COMMAND-STATE-P."
  (unless (equal command-state-p global-hl-line-mode)
    (global-hl-line-mode (if command-state-p 1 0)))
  (modify-all-frames-parameters `((cursor-type . ,(if command-state-p 'box 'bar)))))

(add-hook 'jacob-modal-editing-hook #'jacob-modal-editing-hook-f)

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

(jacob-modal-editing-mode 1)
(jacob-modal-editing-enable)

(provide 'jacob-modal-editing-config)

;;; jacob-modal-editing-config.el ends here
