(defun $projectile-after-switch-project-hook ()
  (when (featurep 'go-mode)
    (go-set-project)))

(req-package projectile
  :hook ((emacs-startup . projectile-mode)
         (projectile-after-switch-project . $projectile-after-switch-project-hook))
  :custom
  (projectile-require-project-root nil)
  ;; https://github.com/emacs-lsp/lsp-python/issues/28#issuecomment-437599058
  ;; https://github.com/emacs-lsp/lsp-mode/pull/470#issuecomment-437600636
  (projectile-completion-system 'ivy)
  )
