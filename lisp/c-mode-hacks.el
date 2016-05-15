;;; c-mode-hacks --- Configure c-mode

;;; Commentary:

;;; Code:

(require 'cc-mode) ;; for objc-mode-map

(setq-default c-basic-offset 4)

(defun c-mode-customizations ()
  "Customize `c-mode'."
  (define-key objc-mode-map (kbd "C-c C-w") 'copy-word-under-cursor)
  (define-key objc-mode-map (kbd "C-c /") 'uncomment-region))

(add-hook 'c-mode-hook 'c-mode-customizations)

(provide 'c-mode-hacks)
;;; c-mode-hacks.el ends here
