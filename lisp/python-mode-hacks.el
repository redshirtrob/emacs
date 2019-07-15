;;; python-mode-hacks --- Customizations to python-mode

;;; Commentary:
;;; May need to eval (jedi:start-server)

;;; Code:

(require 'jedi)

(setq py-python-command "/usr/bin/python3.7")
(setq jedi:environment-root "jedi")
(setq jedi:environment-virtualenv
      (append python-environment-virtualenv
              '("--python" "/usr/bin/python3.7")))

(jedi:start-server)

(defun python-mode-hacks ()
  "Configure `python-mode'."
  (add-to-list 'company-backends 'company-jedi)
  (local-set-key (kbd "M-.") 'dumb-jump-go)
  (local-set-key (kbd "M-,") 'dumb-jump-back)
  (local-set-key (kbd "C-]") 'jedi:complete)
  (setq flycheck-checker 'pylint
        flycheck-checker-error-threshold 900
        flycheck-pylintrc "~/.pylintrc"))

(add-hook 'python-mode-hook 'python-mode-hacks)
(add-hook 'python-mode-hook 'jedi:setup)

(setq py-load-pymacsp nil)

(setq jedi:complete-on-dot t)

(provide 'python-mode-hacks)
;;; python-mode-hacks.el ends here
