;;; ropemacs-hacks --- Customizations to ropemacs

;;; Commentary:
;;; Required pymacs which must be installed with the following on (OS X)
;;; $ pip install -e "git+https://github.com/pinard/Pymacs.git#egg=Pymacs" --src "/usr/local/lib/python2.7/site-packages"
;;; $ cd /usr/local/lib/python2.7/site-packages/pymacs
;;; $ make

;;; Code:

(defun load-ropemacs ()
  "Load pymacs and ropemacs."
  (interactive)

  (require 'ropemacs)
  (require 'pymacs)
  ;; My "C-x p" conflicts with ropemacs, unset it here
  (global-set-key (kbd "C-x p") nil)
  
  (require 'pymacs)
  (pymacs-load "ropemacs" "rope-")
  (setq ropemacs-global-prefix "C-x t")
  
  ;; And reset it here
  (global-set-key (kbd "C-x p") 'select-previous-window)
  
  ;; Automatically save project python buffers before refactorings
  (setq ropemacs-confirm-saving 'nil))
(global-set-key "\C-xtl" 'load-ropemacs)

(provide 'ropemacs-hacks)
;;; ropemacs-hacks.el ends here
