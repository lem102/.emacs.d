;;; Package --- Summary

;; A really verbose description of the current date and time.

;;; Commentary:

;; Just read the code old man

;;; Code:

(defun jacob-day-suffix (day)
  "Return the suffix for a DAY, e.g. a DAY of 17 would return th."
  (if (or (= day 11)
          (= day 12)
          (= day 13))
      "th"
    (let ((unit (% day 10)))
      (if (= unit 1)
          "st"
        (if (= unit 2)
            "nd"
          (if (= unit 3)
              "rd"
            "th"))))))

(defun jacob-long-time-minutes (minute)
  "Return MINUTEs for jacob long time."
  (if (= minute 0)
      ""
    (if (= minute (or 1 59))
        "1 minute "
      (if (= minute 30)
          "Half "
        (if (or (= minute 15) (= minute 45))
            "Quarter "
          (let ((minute-string " minutes ")
                (and-string " and "))
            (if (and (< minute 30) (= (% minute 10) 0))
                (concat (number-to-string minute) minute-string)
              (if (< minute 30)
                  (concat (number-to-string (% minute 10)) (if (> minute 10)
                                                               (concat and-string (number-to-string (- minute (% minute 10))) minute-string)
                                                             minute-string))
                (if (and (> minute 30) (= (% minute 10) 0))
                    (concat (number-to-string (- 60 minute)) minute-string)
                  (concat (number-to-string (- 10 (% minute 10))) (if (< minute 50)
                                                                      (concat and-string (number-to-string (- 50 (- minute (% minute 10)))) minute-string)
                                                                    minute-string)))))))))))

(defun jacob-long-time-past-or-to (minute)
  "Return past or to depending on the value of MINUTE."
  (if (= minute 0)
      ""
    (if (< minute 31)
        "past "
      "to ")))

(defun jacob-long-time-display-hour (hour minute)
  ""
  (let ((shifted-hour (if (< minute 31)
                          hour
                        (+ hour 1))))
    (if (> shifted-hour 12)
        (- shifted-hour 12)
      shifted-hour)))

(defun jacob-long-time-hour (hour minute)
  ""
  (let ((display-hour (jacob-long-time-display-hour hour minute)))
    (if (or (= display-hour 0) (and (= hour 23) (> minute 30)))
        "Midnight"
      (if (or (= display-hour 12) (and (= hour 11) (> minute 30)))
          "Midday"
        (if (< hour 12)
            (concat (number-to-string display-hour) " in the morning")
          (if (< hour 18)
              (concat (number-to-string display-hour) " in the afternoon")
            (concat (number-to-string display-hour) " in the evening")))))))

(defun jacob-long-time (hour minute)
  "Return an overly complex string for the time with input of HOUR and MINUTE."
  (concat (jacob-long-time-minutes minute)
          (jacob-long-time-past-or-to minute)
          (jacob-long-time-hour hour minute)))

;;;###autoload
(defun jacob-display-time ()
  "Display the current date and time in the echo area."
  (interactive)
  (message (concat (format-time-string "%A the %e")
                   (jacob-day-suffix (string-to-number (format-time-string "%e")))
                   (format-time-string " of %B, the year of our Lord %Y, ")
                   "at "
                   (jacob-long-time (string-to-number (format-time-string "%H"))
                                    (string-to-number (format-time-string "%M")))
                   ".")))

(provide 'jacob-long-time)
;;; jacob-long-time.el ends here
