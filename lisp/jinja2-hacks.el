;;; jinja2-hacks --- Configure Jinja2

;;; Commentary:

;;; Code:

(setq auto-mode-alist
      (cons '("\\.j2$" . jinja2-mode) auto-mode-alist))

(add-hook 'jinja2-mode-hook 'turn-on-auto-fill)

(provide 'jinaj2-hacks)
;;; jinja2-hacks.el ends here
