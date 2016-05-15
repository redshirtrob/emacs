;;; go-mode-hacks --- Configure Go

;;; Commentary:

;;; Code:

(require 'go-mode)

(add-hook 'before-save-hook #'gofmt-before-save)

(provide 'go-mode-hacks)
;;; go-mode-hacks.el ends here
