;;; web-mode-hacks --- Customizations to web-mode

;;; Commentary:

;;; Code:

(require 'web-mode)

(defun web-mode-customizations ()
  "Customize web-mode."
  (setq web-mode-enable-part-face t)
  (set-face-attribute 'web-mode-html-tag-face nil :foreground "Pink3")
  (set-face-attribute 'web-mode-html-tag-bracket-face nil :foreground "Black")
  (set-face-attribute 'web-mode-html-attr-equal-face nil :foreground "Black")
  (set-face-attribute 'web-mode-html-attr-name-face nil :foreground "Yellow")
  (set-face-attribute 'web-mode-part-string-face nil :foreground "Red")
  (set-face-attribute 'web-mode-part-face nil :foreground "Red")
  (setq web-mode-markup-indent-offset 2))

(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.nunjucks?\\'" . web-mode))
(setq web-mode-engines-alist '(("django" . "\\.nunjucks\\'")))

;; React Hacks
(add-to-list 'auto-mode-alist '("\\react.*.js[x]?\\'" . web-mode))
(setq web-mode-content-types-alist '(("jsx"  . "\\react.*.js[x]?\\'")))

(add-hook 'web-mode-hook 'web-mode-customizations)

(require 'flycheck)

;; disable jshint since we prefer eslint checking
(setq-default flycheck-disabled-checkers
              (append flycheck-disabled-checkers
                      '(javascript-jshint)))

;; use eslint with web-mode for jsx files
(flycheck-add-mode 'javascript-eslint 'web-mode)

;; customize flycheck temp file prefix
(setq-default flycheck-temp-prefix ".flycheck")

;; disable json-jsonlist checking for json files
(setq-default flycheck-disabled-checkers
              (append flycheck-disabled-checkers
                      '(json-jsonlist)))

;; https://github.com/purcell/exec-path-from-shell
;; only need exec-path-from-shell on OSX
;; this hopefully sets up path and other vars better
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

(require 'flycheck-tip)
(flycheck-tip-use-timer 'verbose)

(provide 'web-mode-hacks)
;;; web-mode-hacks.el ends here



