;;; gdscript-docs.el --- Open documentation in Godot -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020 GDQuest and contributors
;;
;; Author: Josef Vlach <vlach.josef@gmail.com>
;; URL: https://github.com/GDQuest/emacs-gdscript-mode/
;; Version: 1.0.0
;; Package-Requires: ((emacs "26.3"))
;; Maintainer: nathan@gdquest.com
;; Created: May 2020
;; Keywords: languages
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;;  Browse the Godot API reference in the text-based browser eww.
;;
;;; Code:

(require 'eww)
(require 'gdscript-customization)

;;;###autoload
(defun gdscript-docs-browse-api ()
  "Open the main page of Godot API in eww browser."
  (interactive)
  (if (not (string= gdscript-docs-local-path ""))
      (eww-open-file (concat (file-name-as-directory gdscript-docs-local-path) "classes/index.html"))
    (eww-browse-url "https://docs.godotengine.org/en/stable/classes/index.html?#godot-api")))

(defun gdscript-docs-browse-symbol-at-point ()
  "Open the API reference for the symbol at point in the browser eww.
If a page is already open, switch to its buffer."
  (interactive)
  (let* ((symbol (downcase (thing-at-point 'symbol t)))
         (buffer
          (seq-find
           (lambda (current-buffer)
             (with-current-buffer current-buffer
               (when (derived-mode-p 'eww-mode)
                 (string-suffix-p symbol (plist-get eww-data :url) t)
                 ))) (buffer-list))))
    (if buffer (pop-to-buffer-same-window buffer)
      (if (not (string= gdscript-docs-local-path ""))
          (eww-open-file (concat (file-name-as-directory gdscript-docs-local-path) (file-name-as-directory "classes") "class_" symbol ".html"))
        (eww-browse-url (format "https://docs.godotengine.org/en/stable/classes/class_%s.html#%s" symbol symbol) t)))))

(defun gdscript-docs--rename-eww-buffer ()
  "Rename the eww buffer visiting the Godot documentation.
Rename the buffer from a generic name to a name based on the web page's title."
  (when (derived-mode-p 'eww-mode)
    (let ((title (plist-get eww-data :title)))
      (when (string-match "Godot Engine" title)
        (rename-buffer (format "*eww - %s*" title) t)))))

(defun gdscript-docs--filter-content-to-main-div ()
  "Filters a page in the Godot docs down to its main <div>.

This is a re-implementation of `eww-readable'."
  (let* ((old-data eww-data)
         (dom (with-temp-buffer
                (insert (plist-get old-data :source))
                (condition-case nil
                    (decode-coding-region (point-min) (point-max) 'utf-8)
                  (coding-system-error nil))
                (libxml-parse-html-region (point-min) (point-max))))
         (base (plist-get eww-data :url))
         ;; Filters the page down to the main div: <div role="main"> ... </div>
         (main (dom-elements dom 'role "main")))
    (eww-display-html nil nil
                      (list 'base (list (cons 'href base))
                            main)
                      nil (current-buffer))
    (dolist (elem '(:source :url :title :next :previous :up))
      (plist-put eww-data elem
                 (plist-get old-data elem)))
    (eww-update-header-line-format)))

(defun gdscript-docs--eww-follow-link (orig-fun &rest args)
  "Remember url when following local link on a page.

ORIG-FUN is function we wrap around.  ARGS are argument to ORIG-FUN function."
  (let ((url (plist-get eww-data :url))
        (res (apply orig-fun args)))
    (plist-put eww-data :url url)
    res))

(defun gdscript-docs--eww-setup ()
  "Convenience setup for pages with Godot documentation."
  (setq multi-isearch-next-buffer-function nil)
  (gdscript-docs--rename-eww-buffer)
  (gdscript-docs--filter-content-to-main-div))

(add-hook 'eww-after-render-hook #'gdscript-docs--eww-setup)

(advice-add 'eww-follow-link :around #'gdscript-docs--eww-follow-link)

(provide 'gdscript-docs)

;;; gdscript-docs.el ends here
