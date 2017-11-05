(defun $projectile-after-switch-project-hook ()
  (when (featurep 'go-mode)
    (go-set-project)))

(req-package projectile
  :init
  (add-hook 'projectile-after-switch-project-hook
            '$projectile-after-switch-project-hook))
