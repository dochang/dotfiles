(req-package time
  :hook (emacs-startup . display-time)
  :init
  (setq display-time-24hr-format t)
  (setq display-time-day-and-date t))
