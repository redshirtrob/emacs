;; May need to eval (jedi:start-server)

(require 'jedi)
(jedi:start-server)

(defun python-mode-hacks ()
  (add-to-list 'company-backends 'company-jedi)
  (local-set-key (kbd "C-]") 'jedi:complete))

(add-hook 'python-mode-hook 'python-mode-hacks)
(add-hook 'python-mode-hook 'jedi:setup)

(setq py-load-pymacsp nil)

(setq jedi:complete-on-dot t)
