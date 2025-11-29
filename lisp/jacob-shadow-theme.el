;;; jacob-shadow-theme.el --- Black and red theme for programming at night.  -*- lexical-binding: t; -*-

;;; Commentary:
;; 

;;; Code:

(deftheme jacob-shadow
  "Theme that unintentionally resembles a certain hedgehog. For programming at night.")

(let* ((red "#ff0000")
       (red3 "#330000")
       (red5 "#550000")
       (red10 "#aa0000")
       (black "#000000")
       (redblack `(:foreground ,red :background ,black)))
  (custom-theme-set-faces
   'jacob-shadow
   `(default ((t ,redblack)))
   `(cursor ((t (:background ,red))))
   `(region ((t (:background ,red5))))
   `(highlight ((t (:foreground ,red :background ,red3))))
   `(isearch ((t (:background ,red5)))) ; TODO: how to prevent matches from being blue?
   `(button ((t (,@redblack :underline t))))
   `(link ((t (,@redblack :underline t))))
   `(mode-line ((t (:background ,red3))))
   `(mode-line-inactive ((t nil)))
   `(minibuffer-prompt ((t nil)))
   `(fringe ((t nil)))
   `(show-paren-match ((t (:background ,red10))))
   `(shadow ((t nil)))
   `(error ((t (:weight bold))))

   ;; font lock
   `(font-lock-comment-face ((t (:bold t :underline t))))
   `(font-lock-string-face ((t (,@redblack :slant italic))))
   `(font-lock-keyword-face ((t ,redblack)))
   `(font-lock-variable-name-face ((t ,redblack)))
   `(font-lock-type-face ((t ,redblack)))
   `(font-lock-constant-face ((t ,redblack)))
   `(font-lock-function-name-face ((t (,@redblack :bold t))))

   ;; flymake
   `(flymake-warning ((t (:underline (:style dashes)))))

   ;; help
   `(help-key-binding ((t (:background ,red3 :box (:line-width (-1 . -1))))))

   ;; external
   ;; magit
   `(magit-section-highlight ((t (:background ,red3))))
   `(magit-branch-local ((t (:bold t))))
   `(magit-branch-remote ((t (:slant italic))))
   `(magit-section-heading ((t (:foreground nil))))
   ;; TODO: other magit faces
   ))

(provide-theme 'jacob-shadow)

;;; jacob-shadow-theme.el ends here


