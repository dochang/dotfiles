;;; Appointments

(req-package appt
  :ensure nil
  :hook (emacs-startup . appt-activate)
  :init
  ;; Do not beep in `appt-display-message' since `visible-bell' is set
  ;; to `t'.
  (setq appt-audible nil))
