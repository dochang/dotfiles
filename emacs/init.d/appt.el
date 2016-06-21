;;; Appointments

(req-package appt
  :loader :path
  :init
  ;; Do not beep in `appt-display-message' since `visible-bell' is set
  ;; to `t'.
  (setq appt-audible nil))
