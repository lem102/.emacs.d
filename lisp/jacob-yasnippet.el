;;; jacob-yasnippet.el --- Configuration for yasnippet

;;; Commentary:
;;

;;; Code:

(defun jacob-point-in-text-p ()
  "Return t if in comment or string.  Else nil."
  (let ((xsyntax-state (syntax-ppss)))
    (or (nth 3 xsyntax-state)
        (nth 4 xsyntax-state))))

(defun jacob-point-in-code-p ()
  "Return t if outside of string or comment.  Else nil."
  (not (jacob-point-in-text-p)))

(defun jacob-autoinsert-yas-expand ()
  "Replace text in yasnippet template."
  (yas-expand-snippet (buffer-string) (point-min) (point-max)))

(defun jacob-yas-camel-case (input)
  "Convert INPUT to camel case e.g. apple banana -> appleBanana.
For use in yasnippets."
  (let* ((space-at-end (if (string-match-p " $" input) " " ""))
         (words (split-string input))
         (capitalised-words (seq-reduce (lambda (previous current)
                                          (concat previous (capitalize current)))
                                        (cdr words)
                                        (car words))))
    (concat capitalised-words space-at-end)))

(defun jacob-yas-pascal-case (input)
  "Convert INPUT to pascal case e.g. apple banana -> AppleBanana.
For use in yasnippets."
  (let ((space-at-end (if (string-match-p " $" input)
                          " "
                        "")))
    (with-temp-buffer
      (insert input)
      (goto-char (point-min))
      (subword-mode 1)
      (while (not (= (point) (point-max)))
        (call-interactively #'capitalize-word))
      (goto-char (point-min))
      (while (search-forward " " nil "NOERROR")
        (replace-match ""))
      (goto-char (point-max))
      (insert space-at-end)
      (buffer-substring-no-properties (point-min) (point-max)))))

(defun jacob-yas-snake-case (input)
  "Convert INPUT to snake case e.g. apple banana -> apple_banana.
For use in yasnippets."
  (string-replace " " "_" input))

(defun jacob-yas-screaming-snake-case (input)
  "Convert INPUT to screaming snake case e.g. apple banana -> APPLE_BANANA.
For use in yasnippets."
  (upcase (string-replace " " "_" input)))

(defun jacob-yas-kebab-case (input)
  "Convert INPUT to kebab case e.g. apple banana -> apple_banana.
For use in yasnippets."
  (string-replace " " "-" input))

(defvar-keymap jacob-yas-map
  "n" #'yas-new-snippet
  "v" #'yas-visit-snippet-file
  "i" #'yas-insert-snippet)

(defun jacob-yasnippet-config ()
  "Configuration for `yasnippet'."
  (yas-global-mode 1)

  (jacob-defhookf snippet-mode-hook
    (setq-local auto-save-visited-mode nil))

  (keymap-set jacob-xfk-map "y" `("Yasnippet" . ,jacob-yas-map)))

(use-package yasnippet
  :ensure t
  :demand
  :config
  (jacob-yasnippet-config)
  :custom
  (yas-new-snippet-default "# -*- mode: snippet -*-
# key: $1
# --
$0`(yas-escape-text yas-selected-text)`"))

(provide 'jacob-yasnippet)

;;; jacob-yasnippet.el ends here
