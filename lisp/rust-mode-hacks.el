;;; rust-mode-hacks --- Customizations to rust-mode

;;; Commentary:
;;;
;;; http://manenko.com/2016/08/03/setup-emacs-for-rust-development.html
;;;
;;; Install racer (https://github.com/racer-rust/racer#installation)
;;; $ cargo install racer
;;; $ rustup component add rust-src
;;;
;;; Code:

(require 'company)
(require 'racer)
(require 'rust-mode)
(require 'electric)
(require 'eldoc)
(require 'flycheck)
(require 'flycheck-rust)

(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))
(add-hook 'rust-mode-hook  #'company-mode)
(add-hook 'rust-mode-hook  #'racer-mode)
(add-hook 'racer-mode-hook #'eldoc-mode)
(add-hook 'flycheck-mode-hook #'flycheck-rust-setup)
(add-hook 'rust-mode-hook
          '(lambda ()
	     (setq racer-cmd (concat (getenv "HOME") "/.cargo/bin/racer"))
	     (setq racer-rust-src-path (getenv "RUST_SRC_PATH"))
             (local-set-key (kbd "TAB") #'company-indent-or-complete-common)
	     (electric-pair-mode 1)))

(provide 'rust-mode-hacks)
;;; rust-mode-hacks.el ends here
