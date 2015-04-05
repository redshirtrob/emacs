(setq-default c-basic-offset 4)

(defun c-mode-customizations ()
  (define-key objc-mode-map (kbd "C-c C-w") 'copy-word-under-cursor)
  (define-key objc-mode-map (kbd "C-c /") 'uncomment-region))

(add-hook 'c-mode-hook 'c-mode-customizations)
