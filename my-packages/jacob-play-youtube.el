;;; jacob-play-youtube.el --- use mpv and youtube-dl to play music

;;; Commentary:
;; 

;;; Code:

(defvar jacob-play-youtube-history nil)

(add-to-list 'savehist-additional-variables 'jacob-play-youtube-history)

;;;###autoload
(defun jacob-play-youtube ()
  "Ask for a string to search.
Use mpv and youtube-dl to play the first video found."
  (interactive)
  (let ((input (completing-read "YouTube: " jacob-play-youtube-history)))
    (add-to-history 'jacob-play-youtube-history input)
    (start-process "youtube"
                   nil
                   "mpv"
                   "--ytdl-format=worstvideo+bestaudio"
                   (concat "https://www.youtube.com"
                           (with-temp-buffer
                             (insert (jacob-web-request-helper (concat "https://www.youtube.com/results?search_query="
                                                                       (string-replace " "
                                                                                       "+"
                                                                                       input))))
                             (goto-char (point-min))
                             (re-search-forward (rx "/watch?v=" (repeat 0 11 anything)))
                             (match-string 0))))))

(provide 'jacob-play-youtube)

;;; jacob-play-youtube.el ends here
