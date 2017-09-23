;;; rust-mode-hacks --- Customizations to rust-mode

;;; Commentary:
;;; Code:

(require 'flycheck)

(add-hook 'flycheck-mode-hook #'flycheck-rust-setup)

(provide 'rust-mode-hacks)
;;; rust-mode-hacks.el ends here
