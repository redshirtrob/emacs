;;; emacs-lisp-mode-hacks --- Configure emacs-lisp

;;; Commentary:

;;; Code:

(setq auto-mode-alist
      (cons '("emacs$" . emacs-lisp-mode) auto-mode-alist))

(defun emacs-lisp-mode-customizations ()
  "Customize `emacs-lisp-mode'."
  (defvar copy-word-under-cursor-regex)
  (setq copy-word-under-cursor-regex "[^[:word:]\\-]"))
(add-hook 'emacs-lisp-mode-hook 'emacs-lisp-mode-customizations)

(provide 'emacs-lisp-mode-hacks)
;;; emacs-lisp-mode-hacks.el ends here
