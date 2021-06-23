(cond
 ((string-equal system-type "windows-nt")
  (when (member "Consolas" (font-family-list))
    (add-to-list 'default-frame-alist '(font . "Consolas"))))
 ((string-equal system-type "darwin")
  (when (member "Menlo" (font-family-list))
    (add-to-list 'default-frame-alist '(font . "Menlo"))))
 ((string-equal system-type "gnu/linux")
  (when (member "DejaVu Sans Mono" (font-family-list))
    (add-to-list 'default-frame-alist '(font . "DejaVu Sans Mono")))))

;; (let* ((default-font-size (font-get (face-attribute 'default :font) :height))
;;        (default-font-family (font-get (face-attribute 'default :font) :family))
;;        (font-name (concat (if jacob-font-family
;;                               (format "%s" jacob-font-family)
;;                             (format "%s" default-font-family))
;;                           "-"
;;                           (if jacob-font-size
;;                               (format "%s" jacob-font-size)
;;                             (format "%s" default-font-size)))))
;;   (add-to-list 'default-frame-alist `(font . ,font-name)))
