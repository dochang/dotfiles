(req-package dashboard
  :init
  (setq dashboard-items '((agenda)))
  :config
  (dashboard-insert-startupify-lists)
  ;; `dashboard-setup-startup-hook' wants to setup `after-init-hook' and
  ;; `emacs-startup-hook'.  But this function runs in `after-init-hook'.  We
  ;; have to manually run the hook function in `after-init-hook'.
  )
