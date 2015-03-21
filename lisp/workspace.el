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

;; Hack `persp-next' and `persp-prev' to wrap until my PR gets merged
(defun persp-next-wrap ()
  "Switch to next perspective (to the right).
Wrap to the beginning if necessary."
  (interactive)
  (let* ((names (persp-names))
         (pos (cl-position (persp-name persp-curr) names)))
    (cond
     ((null pos) (persp-find-some))
     ((= pos (1- (length names))) (persp-switch (nth 0 names)))
     (t (persp-switch (nth (1+ pos) names))))))

(defun persp-prev-wrap ()
  "Switch to previous perspective (to the left).
Wrap to the beginning if necessary."
  (interactive)
  (let* ((names (persp-names))
         (pos (cl-position (persp-name persp-curr) names)))
    (cond
     ((null pos) (persp-find-some))
     ((= pos 0) (persp-switch (nth (1- (length names)) names)))
     (t (persp-switch (nth (1- pos) names))))))

(define-key persp-mode-map (kbd "C-x x n") 'persp-next-wrap)
(define-key persp-mode-map (kbd "C-x x p") 'persp-prev-wrap)
