;;; jacob-tool-bar.el --- Tool bar stuff  -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:

(require 'svg)

(defun jacob-tool-bar-setup ()
  "Setup tool bars."
  (let* ((down-arrow (let* ((svg (svg-create 25 25)))
                       (svg-polygon svg
                                    '((10 . 2)
                                      (15 . 2)
                                      (15 . 14)
                                      (20 . 14)
                                      (12 . 22)
                                      (5 . 14)
                                      (10 . 14))
                                    :stroke-color "black"
                                    :fill-color "green")
                       (svg-image svg))))
    (setq tool-bar-map (make-sparse-keymap))
    (setq secondary-tool-bar-map (make-keymap))

    (jacob-tool-bar-add-top-item "spell" #'ignore 'jacob-ignore1)
    (jacob-tool-bar-add-top-item "spell" #'ignore 'jacob-ignore2)
    (jacob-tool-bar-add-top-item "spell" #'ignore 'jacob-ignore3)

    (jacob-tool-bar-add-top-item "prev-node" #'puni-backward-sexp-or-up-list 'jacob-backward-sexp)
    (jacob-tool-bar-add-top-item "up-arrow" #'previous-line 'jacob-up)
    (jacob-tool-bar-add-top-item "next-node" #'puni-forward-sexp-or-up-list 'jacob-forward-sexp)

    (jacob-tool-bar-add-top-item "search" #'isearch-forward 'jacob-isearch)
    (jacob-tool-bar-add-top-item "bookmark_add" #'expreg-expand 'jacob-mark)
    (jacob-tool-bar-add-top-item "save" #'jacob-tool-bar-display-keyboard 'jacob-display-keyboard)


    (jacob-tool-bar-add-bottom-item "undo" #'undo 'jacob-undo)
    (jacob-tool-bar-add-bottom-item "cut" #'jacob-kill-line 'jacob-kill-line)
    (jacob-tool-bar-add-bottom-item "paste" #'yank 'jacob-yank)

    (jacob-tool-bar-add-bottom-item "left-arrow" #'backward-char 'jacob-left)
    (jacob-tool-bar-add-bottom-item "placeholder" #'next-line 'jacob-down :image down-arrow)
    (jacob-tool-bar-add-bottom-item "right-arrow" #'forward-char 'jacob-right)

    (jacob-tool-bar-add-bottom-item "spell" #'ignore 'jacob-ignore10)
    (jacob-tool-bar-add-bottom-item "spell" #'ignore 'jacob-ignore11)
    (jacob-tool-bar-add-bottom-item "spell" #'ignore 'jacob-ignore12)))

(defun jacob-tool-bar-display-keyboard ()
  "Display the keyboard."
  (interactive)
  (frame-toggle-on-screen-keyboard (selected-frame) nil))

(defun jacob-tool-bar-add-top-item (icon def key &rest props)
  "Add an item to top tool bar.

ICON, DEF, KEY and PROPS are as in `tool-bar-add-item'."
  (apply #'tool-bar-local-item
         icon
         def
         key
         (if jacob-is-android
             secondary-tool-bar-map
           tool-bar-map)
         props)
  (tool-bar--flush-cache))

(defun jacob-tool-bar-add-bottom-item (icon def key &rest props)
  "Add an item to bottom tool bar.

ICON, DEF, KEY and PROPS are as in `tool-bar-add-item'."
  (apply #'tool-bar-local-item
         icon
         def
         key
         (if jacob-is-android
             tool-bar-map
           secondary-tool-bar-map)
         props)
  (tool-bar--flush-cache))

(provide 'jacob-tool-bar)

;;; jacob-tool-bar.el ends here
