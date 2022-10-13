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

(use-package rustic
  :ensure
  :bind (:map rustic-mode-map
			  ("M-j" . lsp-ui-imenu)
			  ("M-?" . lsp-find-references)
			  ("C-c C-c l" . flycheck-list-errors)
			  ("C-c C-c a" . lsp-execute-code-action)
			  ("C-c C-c r" . lsp-rename)
			  ("C-c C-c q" . lsp-workspace-restart)
			  ("C-c C-c Q" . lsp-workspace-shutdown)
			  ("C-c C-c s" . lsp-rust-analyzer-status))

  :config
  ;; uncomment for less flashiness
  ;; (setq lsp-eldoc-hook nil)
  ;; (setq lsp-enalbe-symbol-highlighting nil)
  ;; (setq lsp-signature-auto-activate nil)
  
  ;; uncomment to enable rustfmt on save
  ;; (setq rustic-format-on-save t)
  (add-hook 'rustic-mode-hook 'rk/rustic-mode-hook))

(defun rk/rustic-mode-hook ()
  ;; so that run C-c C-c C-r works without having to confirm, but don't try to
  ;; save rust buffers that are not file visiting. Once
  ;; https://github.com/brotzeit/rustic/issues/253 has been resolved this should
  ;; not longer be necessary.

  (when buffer-file-name
	;;(setq-local buffer-save-without-query t)
	)
  (add-hook 'before-save-hook 'lsp-format-buffer nil t))

(use-package lsp-mode
  :ensure
  :commands lsp
  :custom
  ;; what to use when checking on-save. "check" is default, I prefer clippy
  (lsp-rust-analyzer-cargo-watch-command "clippy") ;; Hrm?
  (lsp-eldoc-render-all t)
  (lsp-idle-delay 0.6)
  ;; enable / disable the hints as you prefer
  (lsp-rust-analyzer-server-display-inlay-hints t)
  (lsp-rust-analyzer-display-lifetime-elision-hints-enable "skip_trivial")
  (lsp-rust-analyzer-display-chaining-hints t)
  (lsp-rust-analyzer-display-lifetime-elision-hints-use-parameter-names nil)
  (lsp-rust-analyzer-display-closure-return-type-hints t)
  (lsp-rust-analyzer-display-parameter-hints nil)
  (lsp-rust-analyzer-display-reborrow-hints nil)
  :config
  (add-hook 'lsp-mode-hook 'lsp-ui-mode))

(use-package lsp-ui
  :ensure
  :commands lsp-ui-mode
  :custom
  (lsp-ui-peek-always-show t)
  (lsp-ui-sideline-show-hover t)
  (lsp-ui-doc-enable nil))

(provide 'rust-mode-hacks)
;;; rust-mode-hacks.el ends here
