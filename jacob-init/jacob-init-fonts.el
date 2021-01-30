(use-package frame
  :config
  (let* ((default-font-size (font-get (face-attribute 'default :font) :height))
         (default-font-family (font-get (face-attribute 'default :font) :family))
         (font-name (concat (if jacob-font-family
                                (format "%s" jacob-font-family)
                              (format "%s" default-font-family))
                            "-"
                            (if jacob-font-size
                                (format "%s" jacob-font-size)
                              (format "%s" default-font-size)))))
    (add-to-list 'default-frame-alist `(font . ,font-name))))
