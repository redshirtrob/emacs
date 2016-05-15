;;; perspective-hacks --- Customizations to `persp-projectile'

;;; Commentary:
;;; Rebind next/prev to wrap perspectives

;;; Code:

(require 'persp-projectile)

(require 'cl)
(defun persp-next-wrap ()
  "Switch to the next perspective (to the right), wrapping if necessary."
  (interactive)
  (let* ((names (persp-names))
         (pos (cl-position (persp-name persp-curr) names)))
    (cond
     ((null pos) (persp-find-some))
     ((= pos (1- (length names))) (persp-switch (nth 0 names)))
     (t (persp-next)))))

(defun persp-prev-wrap ()
  "Switch to the prev perspective (to the left), wrapping if necessary."
  (interactive)
  (let* ((names (persp-names))
         (pos (cl-position (persp-name persp-curr) names)))
    (cond
     ((null pos) (persp-find-some))
     ((= pos 0) (persp-switch (nth (1- (length names)) names)))
     (t (persp-prev)))))

(define-key persp-mode-map (kbd "C-x x n") 'persp-next-wrap)
(define-key persp-mode-map (kbd "C-x x p") 'persp-prev-wrap)

(provide 'perspective-hacks)
;;; perspective-hacks.el ends here
