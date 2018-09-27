(defun $dashboard-mode-hook ()
  ;; Do not wrap lines in `dashboard-mode'.
  (visual-line-mode -1)
  (toggle-truncate-lines 1))

(req-package dashboard
  :hook ((dashboard-mode . $dashboard-mode-hook))
  :custom
  (dashboard-items '((agenda)))
  :config
  (dashboard-insert-startupify-lists)
  ;; `dashboard-setup-startup-hook' wants to setup `after-init-hook' and
  ;; `emacs-startup-hook'.  But this function runs in `after-init-hook'.  We
  ;; have to manually run the hook function in `after-init-hook'.
  )
