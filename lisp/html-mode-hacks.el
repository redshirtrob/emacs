;;; html-mode-hacks --- Configure html mode

;;; Commentary:

;;; Code:

(defun html-mode-customizations()
  (auto-fill-mode -1))

(remove-hook 'html-mode-hook #'turn-on-auto-fill)
(add-hook 'html-mode-hook 'html-mode-customizations)

;; https://github.com/smihica/emmet-mode
(add-hook 'html-mode-hook 'emmet-mode)

(provide 'html-mode-hacks)
;;; html-mode-hacks.el ends here
