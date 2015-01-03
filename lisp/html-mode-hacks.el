(defun html-mode-customizations()
  (auto-fill-mode -1)
  (remove-hook 'html-mode-hook #'turn-on-auto-fill))

(add-hook 'html-mode-hook 'html-mode-customizations)
