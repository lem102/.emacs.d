;;; jacob-sql.el --- Configuration for the sql package.

;;; Commentary:
;; 

;;; Code:

;; (require 'jacob-xah-fly-keys)

(defun jacob-sql-connect ()
  "Wrapper for `sql-connect' to set postgres password.
CONNECTION is the connection settings. If there is only one connection,
use it without prompting."
  (interactive)
  (require 'sql)
  (let ((connection (if (= 1 (length sql-connection-alist))
                        (symbol-name (caar sql-connection-alist))
                      (sql-read-connection "Connection: "))))
    (with-environment-variables
        (("PGPASSWORD" (cadr (assoc 'sql-password
                                    (assoc-string connection
                                                  sql-connection-alist
                                                  t)))))
      (sql-connect connection))))

(defun jacob-sqli-end-of-buffer ()
  "Move point to end of sqli buffer before sending paragraph.

Intended as before advice for `sql-send-paragraph'."
  (with-current-buffer sql-buffer
    (goto-char (point-max))))

(defun jacob-sql-init ()
  "Initialize symbol `sql'."
  ;; TODO: revisit
  ;; (keymap-set jacob-xfk-map "s" #'jacob-sql-connect)
  )

(defun jacob-sql-config ()
  "Configure symbol `sql'."
  (jacob-defhookf sql-interactive-mode-hook
    (when (eq sql-product 'postgres)
      (setq sql-prompt-regexp "^[-[:alnum:]_]*[-=]\\*?[#>] ")
      (setq sql-prompt-cont-regexp "^\\(?:\\sw\\|\\s_\\)*[-(]\\*?[#>] ")))

  (jacob-xfk-bind-for-mode sql-interactive-mode
                           "SPC , d" #'sql-send-paragraph)

  (advice-add #'sql-send-paragraph :before #'jacob-sqli-end-of-buffer))

(use-package sql
  :commands (sql-read-connection)
  :init
  (jacob-sql-init)
  :config
  (jacob-sql-config))

(provide 'jacob-sql)

;;; jacob-sql.el ends here
