(defun jacob-xah-insert-bracket-pair (@left-bracket @right-bracket)
  "Heavily simplified version of Xah's excellent function. 
My usecases differ to his, so I have removed a vast amount of the functionality. 
Now, this function will insert a pair, or wrap the region if it is active.

Original Version can be found here:
URL `http://ergoemacs.org/emacs/elisp_insert_brackets_by_pair.html'"

  (if (use-region-p)
       ; there's active region
      (let (($p1 (region-beginning))
            ($p2 (region-end)))
        (goto-char $p2)
        (insert @right-bracket)
        (goto-char $p1)
        (insert @left-bracket)
        (goto-char (+ $p2 2)))
     ; no text selection
    (let ($p1 $p2)
       ; I believe this is the part i am interested in.
      (setq $p1 (point) $p2 (point))
      (insert @left-bracket @right-bracket)
      (search-backward @right-bracket))))

(defun jacob-xah-insert-paren () (interactive) (jacob-xah-insert-bracket-pair "(" ")"))
(defun jacob-xah-insert-square-bracket () (interactive) (jacob-xah-insert-bracket-pair "[" "]"))
(defun jacob-xah-insert-brace () (interactive) (jacob-xah-insert-bracket-pair "{" "}"))
(defun jacob-xah-insert-ascii-double-quote () (interactive) (jacob-xah-insert-bracket-pair "\"" "\""))
(defun jacob-xah-insert-ascii-single-quote () (interactive) (jacob-xah-insert-bracket-pair "'" "'"))


(defun jacob-insert-plus ()
  (interactive)
  (insert "+"))

(defun jacob-insert-equals ()
  (interactive)
  (insert "="))

(defun jacob-insert-apostrophe ()
  (interactive)
  (insert "'"))

(defun jacob-insert-at ()
  (interactive)
  (insert "@"))

(defun jacob-insert-tilde ()
  (interactive)
  (insert "~"))

(defun jacob-insert-hash ()
  (interactive)
  (insert "#"))
