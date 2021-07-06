(jacob-is-installed 'typescript-mode
  (with-eval-after-load 'typescript-mode

    (setq typescript-indent-level 2)
    (add-to-list 'auto-mode-alist '("\\.js\\'" . typescript-mode))

    (define-skeleton jacob-typescript-skeleton-console-log
      "insert console.log" nil
      > "console.log(" - ");")

    (define-skeleton jacob-typescript-skeleton-if
      "insert if statement" nil
      > "if (" - ") {" \n
      \n
      -2 "}")
    
    (when (boundp 'typescript-mode-abbrev-table)
      (clear-abbrev-table typescript-mode-abbrev-table))
    
    (define-abbrev-table 'typescript-mode-abbrev-table
      '(
        ("cl" "" jacob-typescript-skeleton-console-log)
        ("if" "" jacob-typescript-skeleton-if)
        ))))
