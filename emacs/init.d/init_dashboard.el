(req-package dashboard
  :init
  (setq dashboard-items '((agenda)))
  :config
  (dashboard-setup-startup-hook))
