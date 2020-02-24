;;; vertigo-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "vertigo" "vertigo.el" (0 0 0 0))
;;; Generated autoloads from vertigo.el

(autoload 'vertigo-jump-down "vertigo" "\
Jump down a number of lines using the home row keys.

\(fn)" t nil)

(autoload 'vertigo-jump-up "vertigo" "\
Jump up a number of lines using the home row keys.

\(fn)" t nil)

(autoload 'vertigo-visible-jump-down "vertigo" "\
Jump down a number of visible lines using the home row keys.

\(fn)" t nil)

(autoload 'vertigo-visible-jump-up "vertigo" "\
Jump up a number of visible lines using the home row keys.

\(fn)" t nil)

(autoload 'vertigo-visual-jump-down "vertigo" "\
Jump down a number of visual lines using the home row keys.

\(fn)" t nil)

(autoload 'vertigo-visual-jump-up "vertigo" "\
Jump up a number of visual lines using the home row keys.

\(fn)" t nil)

(autoload 'vertigo-set-digit-argument "vertigo" "\
Set a positive digit argument using vertigo keys.
If ARG is non-nil, set a negative count.

\(fn ARG)" t nil)

(autoload 'vertigo-run-command-with-digit-argument "vertigo" "\
Like `vertigo-set-digit-argument', but the command is chosen first.
After keys have been pressed that correspond to a command, vertigo will prompt
to set the digit argument for that command and then run it.

\(fn COMMAND-KEYS)" t nil)

(autoload 'vertigo-alt-run-command-with-digit-argument "vertigo" "\
Like `vertigo-run-command-with-digit-agument' but properly messages.
A message showing the command and the prefix arg will be displayed in the
echo area afterwards. M-[0-9] must be mapped to `digit-argument' for this
command to work correctly.

\(fn COMMAND-KEYS)" t nil)

(autoload 'vertigo-set-negative-digit-argument "vertigo" "\
Set a negative digit argument using vertigo keys.
If ARG is non-nil, set a positive count.

\(fn ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "vertigo" '("vertigo-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; vertigo-autoloads.el ends here
