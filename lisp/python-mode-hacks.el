;;; python-mode-hacks --- Customizations to python-mode

;;; Commentary:
;;; May need to eval (jedi:start-server)
;;; May need to eval (jedi:start-dedicated-server) instead

;;; Code:
(require 'lsp-jedi)

(defun always-do-indent (_char)
  "Wrapper for `do-indent'."
  'do-indent)

(defun python-mode-hacks ()
  "Configure `python-mode'."
  (use-package lsp-jedi
    :ensure t
    :config

	(local-set-key (kbd "C-t") 'indent-rigidly-right-to-tab-stop)
    (with-eval-after-load "lsp-mode"
      (add-to-list 'lsp-disabled-clients 'pyls)
      (define-key key-translation-map (kbd "C-c s") 'event-apply-super-modifier))
    (add-hook 'python-mode-hook 'lsp)))

(add-hook 'python-mode-hook 'python-mode-hacks)
(add-hook 'electric-indent-functions #'always-do-indent nil t)

(provide 'python-mode-hacks)
;;; python-mode-hacks.el ends here
