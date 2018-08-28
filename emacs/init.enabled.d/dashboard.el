(req-package dashboard
  :init
  (setq dashboard-items '((agenda)))
  :config
  (when after-init-time
    (dashboard-insert-startupify-lists))
  ;; `dashboard' wants to run `dashboard-insert-startupify-lists' in
  ;; `after-init-hook'.  But we can't, since the library would not be loaded at
  ;; that time.  We explicitly run this function after `dashboard' loaded.
  (dashboard-setup-startup-hook))
