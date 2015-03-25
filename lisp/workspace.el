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

;; Perspective specific scratch buffers don't get associated
;; with the proper projects, because the scratch buffer's
;; `default-directory' is rarely the project's directory.
;;
;; This only works for projects in `ws-root'.  All other
;; scratch buffers will need to be moved over manually by
;; invoking `cd' on the buffer.
(defun fixup-scratch-buffer-hook ()
  "Make sure the perspective scratch buffers's
`default-directory' matches the project root."
  (let* ((name (persp-name persp-curr))
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
  (let* ((name (persp-name persp-curr))
         (shell-name (concat "*shell* (" name ")")))
    (shell shell-name)))
(define-key persp-mode-map (kbd "C-x x l") 'persp-switch-to-shell)

(defun persp-switch-to-ansi-term ()
  "Create an ansi-term unique to the current perspective.  Requires
`zsh-path' to be the path to the zsh executable."
  (interactive)
  (let* ((name (persp-name persp-curr))
         (ansi-name (concat "ansi-term (" name ")"))
         (ansi-buffer-name (concat "*" ansi-name "*"))
         (ansi-buffer (get-buffer ansi-buffer-name)))
    (if (eq ansi-buffer nil)
          (ansi-term zsh-path ansi-name)
        (switch-to-buffer ansi-buffer))))

(define-key persp-mode-map (kbd "C-x x z") 'persp-switch-to-ansi-term)
