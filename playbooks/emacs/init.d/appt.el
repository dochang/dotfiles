;;; Appointments

(req-package appt
  :ensure (appt :pin :built-in)
  :hook (emacs-startup . appt-activate)
  :init
  (setq appt-audible nil)
  ;; Do not beep in `appt-display-message' since `visible-bell' is set
  ;; to `t'.
  )
