# May need to eval (jedi:start-server)

(defun python-mode-hacks ()
  (add-to-list 'company-backends 'company-jedi)
  (local-set-key (kbd "C-]") 'jedi:complete))

(add-hook 'python-mode-hook 'python-mode-hacks)
(add-hook 'python-mode-hook 'jedi:setup)

(setq jedi:complete-on-dot t)
