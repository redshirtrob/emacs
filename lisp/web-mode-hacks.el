;;; web-mode-hacks --- Customizations to web-mode

;;; Commentary:

;;; Code:

(setq web-mode-extra-keywords '(("aspx" . ("endif" "endfor" "endblock" "extends"))))
(setq web-mode-enable-current-element-highlight t)

(require 'web-mode)

(setq js-indent-level 2)

(defun web-mode-customizations ()
  "Customize web-mode."
  (setq web-mode-enable-part-face t)
  (set-face-attribute 'web-mode-html-tag-face nil :foreground "blue")
  (set-face-attribute 'web-mode-keyword-face nil :weight 'ultra-bold)
  (set-face-attribute 'web-mode-block-delimiter-face nil :foreground "black")
  (set-face-attribute 'web-mode-html-tag-bracket-face nil :foreground "blue")
  (set-face-attribute 'web-mode-html-attr-equal-face nil :foreground "Black")
  (set-face-attribute 'web-mode-html-attr-name-face nil :foreground "Blue")
  (set-face-attribute 'web-mode-part-string-face nil :foreground "Red")
  (set-face-attribute 'web-mode-part-face nil :foreground "Red")
  (setq web-mode-markup-indent-offset 2))

(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.nunjucks?\\'" . web-mode))
(setq web-mode-engines-alist '(("django" . "\\.nunjucks\\'")))

;; React Hacks
(add-to-list 'auto-mode-alist '("\\react.*.js[x]?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.js[x]?\\'" . web-mode))
(setq web-mode-content-types-alist '(("jsx"  . "\\react.*.js[x]?\\'")))

(add-hook 'web-mode-hook
          (lambda ()
            ;; short circuit js mode and just do everything in jsx-mode
            (if (equal web-mode-content-type "javascript")
                (web-mode-set-content-type "jsx")
              (message "now set to: %s" web-mode-content-type))))
(setq web-mode-code-indent-offset 2)

(add-hook 'web-mode-hook 'web-mode-customizations)

(require 'flycheck)

;; disable jshint since we prefer eslint checking
(setq-default flycheck-disabled-checkers
              (append flycheck-disabled-checkers
                      '(javascript-jshint)))

;; use eslint with web-mode for jsx files
(flycheck-add-mode 'javascript-eslint 'web-mode)
(flycheck-add-mode 'javascript-eslint 'js-mode)
(flycheck-add-mode 'javascript-eslint 'javascript-mode)

;; customize flycheck temp file prefix
(setq-default flycheck-temp-prefix ".flycheck")

;; disable json-jsonlist checking for json files
(setq-default flycheck-disabled-checkers
              (append flycheck-disabled-checkers
                      '(json-jsonlist)))

(flycheck-def-config-file-var flycheck-sass-lintrc sass-lint ".sass-lint.yml"
  :safe #'stringp)

(flycheck-define-checker sass-lint
    "A SASS (SCSS) checker using Sass Lint (on Node.js).
See URL `https://github.com/sasstools/sass-lint'."
    :command ("sass-lint"
              "--verbose"
              "--format" "checkstyle"
              (config-file "--config" flycheck-sass-lintrc)
              source)
    :error-parser flycheck-parse-checkstyle
    :modes (sass-mode scss-mode))
(add-to-list 'flycheck-checkers 'sass-lint)

;; Configure SCSS
(require 'sass-mode)
(autoload 'sass-mode "sass-mode")
(add-to-list 'auto-mode-alist '("\\.scss\\'" . sass-mode))
(flycheck-add-mode 'sass-lint 'sass-mode)

;; Configure Keys
(global-set-key (kbd "C-c C-v") 'web-mode-element-close)

(provide 'web-mode-hacks)
;;; web-mode-hacks.el ends here
