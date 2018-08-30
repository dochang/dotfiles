(defun $emacs-startup-hook ()
  (unless **theme-initialized**
    ($theme-initialize))
  ($set-theme)
  (unless (daemonp)
    (server-start)))

(req-package emacs
  :hook ((emacs-startup . $emacs-startup-hook)))
