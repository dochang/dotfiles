(req-package guix-build-log
  :ensure (guix :pin :external)
  :hook ((shell-mode . guix-build-log-minor-mode))
  )
