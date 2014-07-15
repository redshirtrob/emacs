(setq auto-mode-alist
      (cons '("emacs$" . emacs-lisp-mode) auto-mode-alist))

(defun emacs-lisp-mode-customizations ()
  (setq copy-word-under-cursor-regex "[^[:word:]\\-]"))
(add-hook 'emacs-lisp-mode-hook 'emacs-lisp-mode-customizations)
