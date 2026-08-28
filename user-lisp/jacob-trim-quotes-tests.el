(ert-deftest jacob-trim-quotes--trim-test ()
  "Tests for `jacob-trim-quotes--trim'."
  (should (string= (jacob-trim-quotes--trim "apple") "apple"))
  (should (string= (jacob-trim-quotes--trim "\"banana\"") "banana"))
  (should (string= (jacob-trim-quotes--trim "citrus\"") "citrus\""))
  (should (string= (jacob-trim-quotes--trim "\"date") "\"date"))
  (should (string= (jacob-trim-quotes--trim "      \"eel\"") "      eel"))
  (should (string= (jacob-trim-quotes--trim "\"fish\"    ") "fish    ")))

