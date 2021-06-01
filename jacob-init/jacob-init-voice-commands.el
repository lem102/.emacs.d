(require 'xah-fly-keys)

(defun jacob-voice-backward-until-whitespace ()
  (interactive)
  (forward-whitespace -1)
  (forward-char 1))

(defun jacob-voice-forward-until-whitespace ()
  (interactive)
  (forward-whitespace 1)
  (backward-char 1))

(defun jacob-voice-forward-until-non-whitespace ()
  (interactive)
  (skip-syntax-forward "\s\n"))

(defun jacob-voice-backward-until-non-whitespace ()
  (interactive)
  (skip-syntax-backward "\s\n"))

(defun jacob-voice-forward-to-next-word-beginning (repetitions)
  (interactive "p")
  (jacob-voice-struct-action repetitions 'forward 'begin nil nil))

(defun jacob-voice-forward-to-next-word-end (repetitions)
  (interactive "p")
  (jacob-voice-struct-action repetitions 'forward 'end nil nil))

(defun jacob-voice-backward-to-previous-word-beginning (repetitions)
  (interactive "p")
  (jacob-voice-struct-action repetitions 'backward 'begin nil nil))

(defun jacob-voice-backward-to-previous-word-end (repetitions)
  (interactive "p")
  (jacob-voice-struct-action repetitions 'backward 'end nil nil))

(defun jacob-voice-delete-forward-to-next-word-beginning (repetitions)
  (interactive "p")
  (jacob-voice-struct-action repetitions 'forward 'begin nil 'delete))

(defun jacob-voice-struct-action (repetitions direction begin-or-end struct action)
  (let* ((subword-mode 1)
         (left-side-word-boundary (save-excursion
                                    (if (eq direction 'backward)
                                        (backward-char 1))
                                    (re-search-backward "\\b" nil t)))
         (right-side-word-boundary (save-excursion
                                     (if (eq direction 'forward)
                                         (forward-char 1))
                                     (re-search-forward "\\b" nil t)))
         (word-boundary-gap-size (- right-side-word-boundary left-side-word-boundary))
         (current-word-size (length (word-at-point))))
    (if action
        (set-mark (point)))
    (if (or (eq word-boundary-gap-size current-word-size))
        (progn
          (if (eq direction 'forward)
              (forward-word (+ repetitions 1))
            (backward-word (+ repetitions 1)))
          (if (xor (eq direction 'backward) (eq begin-or-end 'begin))
              (if (eq direction 'forward)
                  (backward-word 1)
                (forward-word 1))))
      (progn
        (if (eq direction 'forward)
            (forward-word repetitions)
          (backward-word repetitions))
        (if (xor (eq direction 'backward) (eq begin-or-end 'begin))
            (if (eq direction 'forward)
                (backward-word 1)
              (forward-word 1)))))
    (cond ((eq action 'delete)
           (delete-region (region-beginning) (region-end)))
          ((eq action 'kill)
           (kill-region (region-beginning) (region-end)))
          ((eq action 'copy)
           (kill-ring-save (region-beginning) (region-end))))))

(xah-fly-keys-off)

(global-set-key (kbd "C-z p p") 'jacob-xah-insert-paren)
(global-set-key (kbd "C-z p b") 'jacob-xah-insert-square-bracket)
(global-set-key (kbd "C-z p c") 'jacob-xah-insert-brace)
(global-set-key (kbd "C-z p a") 'jacob-xah-insert-angled-bracket)
(global-set-key (kbd "C-z p q") 'jacob-xah-insert-ascii-double-quote)
(global-set-key (kbd "C-z p s") 'jacob-xah-insert-ascii-single-quote)

(global-set-key (kbd "C-z m f u s") 'jacob-voice-forward-until-whitespace)
(global-set-key (kbd "C-z m b u s") 'jacob-voice-backward-until-whitespace)

(global-set-key (kbd "C-z m f u n s") 'jacob-voice-forward-until-non-whitespace)
(global-set-key (kbd "C-z m b u n s") 'jacob-voice-backward-until-non-whitespace)

(global-set-key (kbd "C-z m w 1") 'jacob-voice-backward-to-previous-word-beginning)
(global-set-key (kbd "C-z m w 2") 'jacob-voice-backward-to-previous-word-end)
(global-set-key (kbd "C-z m w 3") 'jacob-voice-forward-to-next-word-beginning)
(global-set-key (kbd "C-z m w 4") 'jacob-voice-forward-to-next-word-end)

(global-set-key (kbd "C-z d w 1") 'jacob-voice-delete-backward-to-previous-word-beginning)
(global-set-key (kbd "C-z d w 2") 'jacob-voice-delete-backward-to-previous-word-end)
(global-set-key (kbd "C-z d w 3") 'jacob-voice-delete-forward-to-next-word-beginning)
(global-set-key (kbd "C-z d w 4") 'jacob-voice-delete-forward-to-next-word-end)

(defun xah-jacob-kill-word (repetitions)
  (interactive "p")
  (when (use-region-p)
    (delete-region (region-beginning) (region-end)))
  (kill-word repetitions))

(defun xah-jacob-backward-kill-word (repetitions)
  (interactive "p")
  (when (use-region-p)
    (delete-region (region-beginning) (region-end)))
  (backward-kill-word repetitions))

(global-set-key (kbd "C-z r") 'xah-jacob-kill-word)
(global-set-key (kbd "C-z e") 'xah-jacob-backward-kill-word)

(defun xah-jacob-beginning-of-line-or-block (repetitions)
  (interactive "p")
  (dotimes (i repetitions)
    (xah-beginning-of-line-or-block)))

(defun xah-jacob-end-of-line-or-block (repetitions)
  (interactive "p")
  (dotimes (i repetitions)
    (xah-end-of-line-or-block)))

(global-set-key (kbd "C-z h") 'xah-jacob-beginning-of-line-or-block)
(global-set-key (kbd "C-z ;") 'xah-jacob-end-of-line-or-block)

(global-set-key (kbd "C-z c") 'xah-copy-line-or-region)
(global-set-key (kbd "C-z x") 'xah-cut-line-or-region)

(defun jacob-recenter-top ()
  (interactive)
  (recenter 5))

(defun jacob-recenter-centre ()
  (interactive)
  (recenter))

(defun jacob-recenter-bottom ()
  (interactive)
  (recenter -5))

(global-set-key (kbd "C-z C-l t") 'jacob-recenter-top)
(global-set-key (kbd "C-z C-l c") 'jacob-recenter-centre)
(global-set-key (kbd "C-z C-l b") 'jacob-recenter-bottom)

(defun jacob-move-to-window-line-top ()
  (interactive)
  (move-to-window-line 5))

(defun jacob-move-to-window-line-centre ()
  (interactive)
  (move-to-window-line nil))

(defun jacob-move-to-window-line-bottom ()
  (interactive)
  (move-to-window-line -5))

(global-set-key (kbd "C-z M-r t") 'jacob-move-to-window-line-top)
(global-set-key (kbd "C-z M-r c") 'jacob-move-to-window-line-centre)
(global-set-key (kbd "C-z M-r b") 'jacob-move-to-window-line-bottom)


(global-set-key (kbd "C-z v") 'xah-paste-or-paste-previous)
(global-set-key (kbd "C-z SPC v") 'consult-yank-from-kill-ring)

(global-set-key (kbd "C-z 0") 'xah-pop-local-mark-ring)

(global-set-key (kbd "C-z SPC i j") 'consult-recent-file)

(global-set-key (kbd "C-z .") 'xah-forward-right-bracket)
(global-set-key (kbd "C-z m") 'xah-backward-left-bracket)
(global-set-key (kbd "C-z /") 'xah-goto-matching-bracket)

(global-set-key (kbd "C-z SPC e p f") 'projectile-find-file)

(global-set-key (kbd "C-z d") 'xah-delete-backward-char-or-bracket-text)

(defun jacob-voice-mark-command ()
  (interactive)
  (if (region-active-p)
      (er/expand-region 1)
    (set-mark (point))))

(global-set-key (kbd "C-z t") 'jacob-voice-mark-command)

(global-set-key (kbd "C-x 2") 'jacob-split-window-below-select-new)
(global-set-key (kbd "C-x 3") 'jacob-split-window-right-select-new)

(global-set-key (kbd "C-z w") 'xah-shrink-whitespaces)

(defun jacob-switch-to-previous-buffer ()
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

(global-set-key (kbd "C-z f") 'jacob-switch-to-previous-buffer)
(global-set-key (kbd "C-z F") 'ibuffer)
