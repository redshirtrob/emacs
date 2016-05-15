;;; objc-mode-hacks --- Customizations to objc-mode

;;; Commentary:

;;; Code:

(require 'objc-mode)

(setq auto-mode-alist
      (cons '("\\.m$" . objc-mode) auto-mode-alist))
(setq auto-mode-alist
      (cons '("\\.h$" . objc-mode) auto-mode-alist))
(setq auto-mode-alist
      (cons '("\\.mm$" . objc-mode) auto-mode-alist))

(defun objc-in-header-file ()
  "Determine if the buffer is a header file."
  (let* ((filename (buffer-file-name))
         (extension (car (last (split-string filename "\\.")))))
    (string= "h" extension)))

(defun objc-jump-to-extension (extension)
  "Jump to a file with the given `EXTENSION'."
  (let* ((filename (buffer-file-name))
         (file-components (append (butlast (split-string filename
                                                         "\\."))
                                  (list extension))))
    (find-file (mapconcat 'identity file-components "."))))

(defun objc-jump-between-header-source ()
  "Move between a header and implementation file.
Assumes both files are in the same directory."
  (interactive)
  (if (objc-in-header-file)
      (objc-jump-to-extension "m")
    (objc-jump-to-extension "h")))

(defun insert-nsstring-constant (name)
  "Insert an NSString constant with the given `NAME'."
  (interactive
   (let ((constant-name
          (read-string "Enter constant name: " (file-name-base (buffer-file-name)))))
     (insert (format "static NSString *const %s = @\"%s\";\n" constant-name constant-name)))))

(defun objc-mode-customizations ()
  "Customize `objc-mode'."
  (define-key objc-mode-map (kbd "C-c C-w") 'copy-word-under-cursor)
  (define-key objc-mode-map (kbd "C-c /") 'uncomment-region)
  (define-key objc-mode-map (kbd "C-c t") 'objc-jump-between-header-source))

(add-hook 'objc-mode-hook 'objc-mode-customizations)

(provide 'objc-mode-hacks)
;;; objc-mode-hacks.el ends here
