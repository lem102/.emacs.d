;; -*- coding: utf-8; lexical-binding: t; -*-

(clear-abbrev-table global-abbrev-table)

(define-abbrev-table 'global-abbrev-table
  '(

    ))

(when (boundp 'python-mode-abbrev-table)
  (clear-abbrev-table python-mode-abbrev-table))

(define-abbrev-table 'python-mode-abbrev-table
  '(
    ("key" "\"\": Key(\"\"),")
    ))
  
(set-default 'abbrev-mode t)

(setq save-abbrevs nil)
