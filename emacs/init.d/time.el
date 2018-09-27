(req-package time
  :hook (emacs-startup . display-time)
  :custom
  (display-time-24hr-format t)
  (display-time-day-and-date t))
