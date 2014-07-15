(require 'web-mode)

(defun web-mode-customizations ()
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (setq web-mode-enable-part-face t)
  (set-face-attribute 'web-mode-html-tag-face nil :foreground "Pink3")
  (set-face-attribute 'web-mode-html-tag-bracket-face nil :foreground "Black")
  (set-face-attribute 'web-mode-html-attr-equal-face nil :foreground "Black")
  (set-face-attribute 'web-mode-html-attr-name-face nil :foreground "Yellow")
  (set-face-attribute 'web-mode-part-string-face nil :foreground "Red")
  (set-face-attribute 'web-mode-part-face nil :foreground "Red"))

(add-hook 'web-mode-hook 'web-mode-customizations)
