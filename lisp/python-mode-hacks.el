;;; python-mode-hacks --- Customizations to python-mode

;;; Commentary:
;;; May need to eval (jedi:start-server)

;;; Code:

(require 'jedi)
(jedi:start-server)

(defun python-mode-hacks ()
  "Configure `python-mode'."
  (add-to-list 'company-backends 'company-jedi)
  (local-set-key (kbd "M-.") 'dumb-jump-go)
  (local-set-key (kbd "M-,") 'dumb-jump-back)
  (local-set-key (kbd "C-]") 'jedi:complete))

(add-hook 'python-mode-hook 'python-mode-hacks)
(add-hook 'python-mode-hook 'jedi:setup)

(setq py-load-pymacsp nil)

(setq jedi:complete-on-dot t)

(provide 'python-mode-hacks)
;;; python-mode-hacks.el ends here
