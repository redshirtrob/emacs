;;; shell-script-mode-hacks --- Customizations to shell-script-mode

;;; Commentary:

;;; Code:

(setq auto-mode-alist
      (cons '("zshrc$" . shell-script-mode) auto-mode-alist))

(provide 'shell-script-mode-hacks)
;;; shell-script-mode-hacks.el ends here
