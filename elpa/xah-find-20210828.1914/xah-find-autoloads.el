;;; xah-find-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "xah-find" "xah-find.el" (0 0 0 0))
;;; Generated autoloads from xah-find.el

(autoload 'xah-find-count "xah-find" "\
Report how many occurrences of a string, of a given dir.
Similar to `rgrep', but written in pure elisp.
Result is shown in buffer *xah-find output*.
Case sensitivity is determined by `case-fold-search'. Call `toggle-case-fold-search' to change.
\\{xah-find-output-mode-map}

\(fn SEARCHSTR COUNTEXPR COUNTNUMBER INPUTDIR PATHREGEX)" t nil)

(autoload 'xah-find-text "xah-find" "\
Report files that contain string.
By default, not case sensitive, and print surrounding text.
If `universal-argument' is called first, prompt to ask.
Result is shown in buffer *xah-find output*.
\\{xah-find-output-mode-map}

\(fn SEARCHSTR INPUTDIR PATHREGEX FIXEDCASESEARCHQ PRINTCONTEXT-P)" t nil)

(autoload 'xah-find-replace-text "xah-find" "\
Find/Replace string in all files of a directory.
Search string can span multiple lines.
No regex.

Backup, if requested, backup filenames has suffix with timestamp, like this: ~xf20150531T233826~

Result is shown in buffer *xah-find output*.
\\{xah-find-output-mode-map}

\(fn SEARCHSTR REPLACESTR INPUTDIR PATHREGEX WRITETOFILEQ FIXEDCASESEARCHQ FIXEDCASEREPLACEQ &optional BACKUPQ)" t nil)

(autoload 'xah-find-text-regex "xah-find" "\
Report files that contain a string pattern, similar to `rgrep'.
Result is shown in buffer *xah-find output*.
\\{xah-find-output-mode-map}
Version 2016-12-21

\(fn SEARCHREGEX INPUTDIR PATHREGEX FIXEDCASESEARCHQ PRINTCONTEXTLEVEL)" t nil)

(autoload 'xah-find-replace-text-regex "xah-find" "\
Find/Replace by regex in all files of a directory.

Backup, if requested, backup filenames has suffix with timestamp, like this: ~xf20150531T233826~

When called in lisp code:
Regex is a regex pattern.
ReplaceStr is replacement string.
InputDir is input directory to search (includes all nested subdirectories).
PathRegex is a regex to filter file paths.
WriteToFileQ, when true, write to file, else, print a report of changes only.
FixedCaseSearchQ sets `case-fold-search' for this operation.
FixedCaseReplaceQ if true, then the letter-case in replacement is literal. (this is relevant only if FixedCaseSearchQ is true.)
ShowcontexQ print characters before and after match.
BackupQ if ture does backup.

Result is shown in buffer *xah-find output*.
\\{xah-find-output-mode-map}

Version 2018-08-20

\(fn REGEX REPLACESTR INPUTDIR PATHREGEX WRITETOFILEQ FIXEDCASESEARCHQ FIXEDCASEREPLACEQ SHOWCONTEXQ BACKUPQ)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "xah-find" '("xah-find-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; xah-find-autoloads.el ends here
