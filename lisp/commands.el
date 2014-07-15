;; Similar to C-x o
(defun select-previous-window ()
  "Switch to the previous window"
  (interactive)
  (select-window (previous-window)))

(defvar copy-word-under-cursor-regex "[^[:word:]_]"
  "Regular expression to use when copying with `copy-word-under-cursor'.
Can be customized for each major mode.")

(defun copy-word-under-cursor ()
  "Copy the word under the cursor to the kill ring."
  (interactive)
  (save-excursion
    (save-excursion (re-search-backward copy-word-under-cursor-regex))
    (let ((beg (+ (match-beginning 0) 1))
          (end (re-search-forward copy-word-under-cursor-regex)))
      (copy-region-as-kill beg (- end 1)))))
