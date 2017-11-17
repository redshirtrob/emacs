;;; asm-mode-hacks --- Configure c-mode

;;; Commentary:

;;; Code:

(require 'asm-mode) 

(setq auto-mode-alist
      (cons '("\\.MAR$" . asm-mode) auto-mode-alist))

(provide 'asm-mode-hacks)
;;; asm-mode-hacks.el ends here

