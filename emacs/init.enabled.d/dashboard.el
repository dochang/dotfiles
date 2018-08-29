(req-package dashboard
  :hook ((emacs-startup . (lambda () (require 'dashboard)))
         (window-setup . (lambda ()
                           (dashboard-insert-startupify-lists)
                           (switch-to-buffer "*dashboard*")
                           (goto-char (point-min))
                           (redisplay))))
  :init
  (setq dashboard-items '((agenda)))
  :config
  (dashboard-setup-startup-hook)
  ;; `dashboard-setup-startup-hook' wants to setup `after-init-hook' and
  ;; `emacs-startup-hook'.  But the hook functions never run because
  ;; `dashboard' will be loaded when `emacs-startup-hook' is running.  We put
  ;; the setup code in `window-setup-hook'.
  )
