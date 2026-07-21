;;; jacob-tool-bar.el --- Tool bar stuff  -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:

(defun jacob-setup-tool-bars ()
  "Setup tool bars."
  (setq tool-bar-map (make-sparse-keymap))
  (setq secondary-tool-bar-map (make-keymap))

  (jacob-tool-bar-add-top-item "spell" #'ignore 'jacob-ignore1)
  (jacob-tool-bar-add-top-item "spell" #'ignore 'jacob-ignore2)
  (jacob-tool-bar-add-top-item "spell" #'ignore 'jacob-ignore3)

  (jacob-tool-bar-add-top-item "left-arrow" #'backward-char 'jacob-left)
  (jacob-tool-bar-add-top-item "sort-ascending" #'next-line 'jacob-down)
  (jacob-tool-bar-add-top-item "right-arrow" #'forward-char 'jacob-right)

  (jacob-tool-bar-add-top-item "spell" #'ignore 'jacob-ignore4)
  (jacob-tool-bar-add-top-item "spell" #'ignore 'jacob-ignore5)
  (jacob-tool-bar-add-top-item "spell" #'ignore 'jacob-ignore6)


  (jacob-tool-bar-add-bottom-item "spell" #'ignore 'jacob-ignore7)
  (jacob-tool-bar-add-bottom-item "spell" #'ignore 'jacob-ignore8)
  (jacob-tool-bar-add-bottom-item "spell" #'ignore 'jacob-ignore9)

  (jacob-tool-bar-add-bottom-item "prev-node" #'puni-backward-sexp-or-up-list 'jacob-backward-sexp)
  (jacob-tool-bar-add-bottom-item "up-arrow" #'previous-line 'jacob-up)
  (jacob-tool-bar-add-bottom-item "next-node" #'puni-forward-sexp-or-up-list 'jacob-forward-sexp)

  (jacob-tool-bar-add-bottom-item "spell" #'ignore 'jacob-ignore10)
  (jacob-tool-bar-add-bottom-item "spell" #'ignore 'jacob-ignore11)
  (jacob-tool-bar-add-bottom-item "spell" #'ignore 'jacob-ignore12))

(defun jacob-tool-bar-add-top-item (icon def key &rest props)
  "Add an item to top tool bar.

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

(defun jacob-tool-bar-add-bottom-item (icon def key &rest props)
  "Add an item to bottom tool bar.

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

(provide 'jacob-tool-bar)

;;; jacob-tool-bar.el ends here
