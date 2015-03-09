;; Configure helm/perspective/projectile to approximate the
;; concept of workspaces.

(require 'helm)

;; Enable projectile
(projectile-global-mode)
(setq projectile-completion-system 'helm)
(helm-projectile-on)

;; Perspective
(require 'perspective)
(persp-mode)

(require 'persp-projectile)
(define-key projectile-mode-map (kbd "C-x C-p") 'projectile-persp-switch-project)

;; `helm-buffers-list--match-fn' is void, so we alias it to the default
;; match function
(defalias 'helm-buffers-list--match-fn 'helm-buffers-match-function)
(global-set-key (kbd "C-x b") 'helm-projectile-switch-to-buffer)
