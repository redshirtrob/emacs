(require 'company)
(eval-after-load 'company
  '(add-to-list 'company-backends 'company-irony))
(add-hook 'irony-mode-hook 'company-irony-setup-begin-commands)
(add-hook 'after-init-hook 'global-company-mode)

(setq company-idle-delay 0.5)
(setq company-minimum-prefix-length 3)
