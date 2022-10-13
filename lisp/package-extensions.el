;;; package-extensions --- Configure Emacs Package Manager

;;; Commentary:

;;; Code:

(require 'package)

;; Configure additional package archives
(package-initialize)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))

;; Install missing packages
;; http://batsov.com/articles/2012/02/19/package-management-in-emacs-the-good-the-bad-and-the-ugly/
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar local-packages
  '(2048-game ag alchemist async auto-complete caml cider cl-generic
              clojure-mode clojure-mode-extra-font-locking company company-c-headers company-go company-irony
              company-restclient dash deferred dumb-jump
              editorconfig elixir-mode emmet-mode epl 
	      flycheck flycheck-elixir flymake-elixir flycheck-popup-tip flycheck-tip flycheck-rust
              flx flx-ido git-commit go-autocomplete go-mode go-snippets
              handlebars-mode ggtags
              helm helm-ack helm-ag helm-company helm-core helm-dash helm-git helm-gtags helm-projectile helm-themes
              iedit irony magit markdown-mode neotree paredit
              paredit-everywhere persp-mode persp-projectile perspective pkg-info popup projectile
              python-django python-mode queue
	      racer
              restclient know-your-http-well s sbt-mode seq sass-mode scss-mode slime slime-company spinner
              thrift tuareg use-package utop
              virtualenv w3m web-mode with-editor yasnippet zencoding-mode)
  "A list of packages to ensure are installed at launch.")

(require 'cl) ;; Note: if this causes issues, switch to (require 'cl)
(defun local-packages-installed-p ()
  "Check if packages are installed."
  (loop for p in local-packages
        when (not (package-installed-p p)) do (return nil)
        finally (return t)))

(unless (local-packages-installed-p)
  ;; check for new packages (package versions)
  (message "%s" "Emacs is now refreshing its package database...")
  (package-refresh-contents)
  (message "%s" " done.")
  ;; install the missing packages
  (dolist (p local-packages)
    (when (not (package-installed-p p))
      (package-install p))))

(provide 'package-extensions)
;;; package-extensions.el ends here
