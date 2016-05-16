;;; package-extensions --- Configure Emacs Package Manager

;;; Commentary:

;;; Code:

(require 'package)

;; Configure additional package archives
(package-initialize)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))

(add-to-list 'load-path "~/.emacs.d/el-get/el-get")
(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (let (el-get-master-branch)
      (goto-char (point-max))
      (eval-print-last-sexp))))
(el-get 'sync)

;; Install missing packages
;; http://batsov.com/articles/2012/02/19/package-management-in-emacs-the-good-the-bad-and-the-ugly/
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar local-packages
  '(2048-game ack-and-a-half ag async auto-complete autopair caml cider cl-generic
              clojure-mode company company-c-headers company-cmake company-go company-irony
              company-jedi company-restclient concurrent ctable dash deferred diminish
              editorconfig  emmet-mode epc epl flycheck flycheck-elixir flymake-elixir flycheck-tip
              flx flx-ido git-commit go-autocomplete go-mode go-snippets handlebars-mode
              helm helm-ack helm-company helm-core helm-dash helm-git helm-projectile helm-themes
              heroku iedit ipython irony jedi jedi-core magit magit-popup markdown-mode paredit
              paredit-everywhere persp-mode persp-projectile perspective pkg-info popup projectile
              python-django python-environment python-mode python-pylint pyvirtualenv queue
              restclient know-your-http-well s seq sass-mode scss-mode slime slime-company spinner tuareg utop
              virtualenv w3m web-mode with-editor yasnippet zencoding-mode)
  "A list of packages to ensure are installed at launch.")

(require 'cl-lib) ;; Note: if this causes issues, switch to (require 'cl)
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
