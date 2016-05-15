;;; irony-mode-hacks --- Configure irony mode

;;; Commentary:

;;; Code:

;; From https://github.com/Sarcasm/irony-mode

;; Installing on OS X
;; Install llvm with clang
;;  $ brew install llvm --with-clang
;; Run `irony-install-server' and alter cmake command as follows
;;  cmake -DCMAKE_PREFIX_PATH=/usr/local/opt/llvm -DCMAKE_INSTALL_PREFIX\=/Users/rob/.emacs.d/irony/ /Users/rob/.emacs.d/elpa/irony-20150408.1501/server && cmake --build . --use-stderr --config Release --target install

(require 'irony-mode)

(add-hook 'objc-mode-hook 'irony-mode)

;; replace the `completion-at-point' and `complete-symbol' bindings in
;; irony-mode's buffers by irony-mode's function
(defun my-irony-mode-hook ()
  "Configure `irony-mode'."
  (define-key irony-mode-map [remap completion-at-point]
    'irony-completion-at-point-async)
  (define-key irony-mode-map [remap complete-symbol]
    'irony-completion-at-point-async))
(add-hook 'irony-mode-hook 'my-irony-mode-hook)
(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

(provide 'irony-mode-hacks)
;;; irony-mode-hacks.el ends here
