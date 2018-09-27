;;; Appointments

(req-package appt
  :ensure nil
  :hook (emacs-startup . appt-activate)
  :custom
  (appt-audible nil)
  ;; Do not beep in `appt-display-message' since `visible-bell' is set
  ;; to `t'.
  )
