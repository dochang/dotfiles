(defun $emacs-startup-hook ()
  (unless (bound-and-true-p **org-timer**)
    (setq **org-timer** (run-at-time nil 3600 'org-agenda-to-appt)))
  (appt-activate 1)
  (unless **theme-initialized**
    ($theme-initialize))
  ($set-theme)
  (unless (daemonp)
    (server-start)))

(req-package emacs
  :hook ((emacs-startup . $emacs-startup-hook)))
