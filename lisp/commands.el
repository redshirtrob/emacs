;;; commands --- Some custom commands

;;; Commentary:

;;; Code:

(defun select-previous-window ()
  "Switch to the previous window."
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

(defun swap-windows ()
  "Switch the buffers between two windows."
  (interactive)
  (unless (not (eq (length (window-list)) 2))
    (let* ((this-window (selected-window))
           (this-buffer (window-buffer this-window))
           (that-window (next-window))
           (that-buffer (window-buffer that-window))
           (new-this-buffer that-buffer)
           (new-that-buffer this-buffer))
      (set-window-buffer this-window new-this-buffer)
      (set-window-buffer that-window new-that-buffer))))

(defun osx-copy-region-to-clipboard ()
  "Copy current region to the OS X clipboard."
  (interactive)
  (when (eq system-type 'darwin)
    (shell-command-on-region (region-beginning) (region-end) "pbcopy")))

(provide 'commands)
;;; commands.el ends here
