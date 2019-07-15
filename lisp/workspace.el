;;; workspace --- Create a workspace-like environment

;;; Commentary:
;;; Configure helm/perspective/projectile to approximate the
;;; concept of workspaces.

;;; Code:

(require 'helm)

;;; `make-variable-frame-local' is deprecated but perspective-el still
;;; uses it. Until perspective-el gets fixed, this will make it work
;;; in single frame environments.
(when (not (fboundp 'make-variable-frame-local))
  (defun make-variable-frame-local (variable) variable))

;; Enable projectile
(projectile-global-mode)
(setq projectile-completion-system 'helm)

;; Projectile removed the default keymap prefix, so we set it here:
;; https://github.com/bbatsov/helm-projectile/issues/116
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

(helm-projectile-on)

;; Perspective
(require 'perspective)
(persp-mode)

(setq projectile-mode-line '(:eval (format " Prj[%s]" (projectile-project-name))))

(require 'persp-projectile)
(define-key projectile-mode-map (kbd "C-x C-p") 'projectile-persp-switch-project)

;; `helm-buffers-list--match-fn' is void, so we alias it to the default
;; match function
(defalias 'helm-buffers-list--match-fn 'helm-buffers-match-function)

;; Perspective specific scratch buffers don't get associated
;; with the proper projects, because the scratch buffer's
;; `default-directory' is rarely the project's directory.
;;
;; This only works for projects in `ws-root'.  All other
;; scratch buffers will need to be moved over manually by
;; invoking `cd' on the buffer.
(defun fixup-scratch-buffer-hook ()
  "Change the name of scratch buffer.
Make sure the perspective scratch buffers's
`default-directory' matches the project root."
  (let* ((name (persp-name (persp-curr)))
         ;; This should really be accessible via perspective.  If
         ;; the name of the scratch buffer ever changes, this will
         ;; break.
         (buffer-name (concat "*scratch* (" name ")"))
         (buffer (get-buffer buffer-name)))
    (with-current-buffer buffer
      (let ((directory (concat ws-root name)))
        (cd directory)))))

(add-hook 'persp-created-hook 'fixup-scratch-buffer-hook)

(global-set-key (kbd "C-x C-b") 'helm-projectile-switch-to-buffer)

;; I want a separate shell for each perspective
(defun persp-switch-to-shell ()
  "Create a shell unique to the current perspective."
  (interactive)
  (let* ((name (persp-name (persp-curr)))
         (shell-name (concat "*shell* (" name ")")))
    (shell shell-name)))
(define-key persp-mode-map (kbd "C-x x l") 'persp-switch-to-shell)

(defun persp-switch-to-ansi-term ()
  "Create an `ansi-term' unique to the current perspective.
Requires `zsh-path' to be the path to the zsh executable."
  (interactive)
  (let* ((name (persp-name (persp-curr)))
         (ansi-name (concat "ansi-term (" name ")"))
         (ansi-buffer-name (concat "*" ansi-name "*"))
         (ansi-buffer (get-buffer ansi-buffer-name)))
    (if (eq ansi-buffer nil)
          (ansi-term zsh-path ansi-name)
        (switch-to-buffer ansi-buffer))))

(define-key persp-mode-map (kbd "C-x x z") 'persp-switch-to-ansi-term)

(provide 'workspace)
;;; workspace.el ends here
