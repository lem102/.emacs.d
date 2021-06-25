(jacob-is-installed 'sml-mode
  (with-eval-after-load 'sml-mode

    (setq sml-abbrev-skeletons nil)

    (define-skeleton jacob-sml-skeleton-val
      "insert val" nil
      > "val " - " =")

    (define-skeleton jacob-sml-skeleton-if
      "insert if" nil
      > "if " - "" \n
      -4 "then " \n
      -4 "else ")

    (define-skeleton jacob-sml-skeleton-let
      "insert let" nil
      > "let" \n
      - \n
      -4 "in" \n
      -4 "end")

    (define-skeleton jacob-sml-skeleton-fun
      "insert fun" nil
      > "fun " - "() =")

    (define-skeleton jacob-sml-skeleton-case
      "insert case" nil
      > "case " - " of => ")

    (when (boundp 'sml-mode-abbrev-table)
      (clear-abbrev-table sml-mode-abbrev-table))
    
    (define-abbrev-table 'sml-mode-abbrev-table
      '(
        ("val" "" jacob-sml-skeleton-val)
        ("if" "" jacob-sml-skeleton-if)
        ("let" "" jacob-sml-skeleton-let)
        ("fun" "" jacob-sml-skeleton-fun)
        ("case" "" jacob-sml-skeleton-case)
        ))

    ))
