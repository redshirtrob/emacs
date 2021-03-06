;;; ekeys --- Configure key bindings

;;; Commentary:
;;;
;;; Key binding customizations all in one place.

;;; Code:

(global-set-key (kbd "C-x j") 'python-django-open-project)
(global-set-key (kbd "C-c C-w") 'copy-word-under-cursor)
(global-set-key (kbd "C-x p") 'select-previous-window)
(global-set-key (kbd "M-s u") 'revert-buffer)
(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-c\C-m" 'execute-extended-command)
(global-set-key (kbd "C-c y") 'balance-windows)
(global-set-key (kbd "M-C") 'osx-copy-region-to-clipboard)
(global-set-key (kbd "C-c m") 'magit-status)
(global-set-key (kbd "C-c d") 'helm-dash)
(global-set-key (kbd "C-c ;") 'iedit-mode)
(global-set-key (kbd "C-c /") 'comment-region)
(global-set-key (kbd "C-c \\") 'uncomment-region)
(global-set-key (kbd "C-c w") 'whitespace-mode)
(global-set-key (kbd "C-c C-x") 'neotree-toggle)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-c C-c") 'hs-toggle-hiding)

(provide 'ekeys)
;;; ekeys.el ends here
