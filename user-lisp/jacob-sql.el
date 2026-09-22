;;; jacob-sql.el --- Configuration for the sql package.  -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:

(require 'sql)

;;;###autoload
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

;;;###autoload
(defun jacob-sqli-end-of-buffer ()
  "Move point to end of sqli buffer before sending paragraph.

Intended as before advice for `sql-send-paragraph'."
  (with-current-buffer sql-buffer
    (goto-char (point-max))))

(provide 'jacob-sql)

;;; jacob-sql.el ends here
