(defun $projectile-after-switch-project-hook ()
  (when (featurep 'go-mode)
    (go-set-project)))

(req-package projectile
  :hook ((emacs-startup . projectile-mode)
         (projectile-after-switch-project . $projectile-after-switch-project-hook)))
