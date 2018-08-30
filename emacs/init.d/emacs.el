(defun $emacs-startup-hook ()
  (unless **theme-initialized**
    ($theme-initialize))
  ($set-theme))

(req-package emacs
  :hook ((emacs-startup . $emacs-startup-hook)))
