;;; jacob-play-youtube.el --- use mpv and youtube-dl to play music

;;; Commentary:
;; 

;;; Code:

(defvar jacob-play-youtube-history-file "~/.emacs.d/jacob-play-youtube")

(defvar jacob-play-youtube-history (when (file-exists-p jacob-play-youtube-history-file)
                                     (with-temp-buffer
                                       (insert-file-contents-literally jacob-play-youtube-history-file)
                                       (split-string (substring-no-properties (buffer-string)) "\n"))))

(defun jacob-play-youtube-write-history ()
  "Write current `jacob-play-youtube-history' to file."
  (with-temp-buffer
    (insert (string-join jacob-play-youtube-history "\n"))
    (append-to-file (point-min) (point-max) jacob-play-youtube-history-file)))

(add-hook 'kill-emacs-hook #'jacob-play-youtube-write-history)

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
                             (re-search-forward "/watch\\?v=.\\{0,11\\}")
                             (match-string 0))))))

(provide 'jacob-play-youtube)

;;; jacob-play-youtube.el ends here
