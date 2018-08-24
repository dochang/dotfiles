(defun $projectile-after-switch-project-hook ()
  (when (featurep 'go-mode)
    (go-set-project)))

(req-package projectile
  :hook (projectile-after-switch-project . $projectile-after-switch-project-hook))
