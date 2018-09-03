;;; Dired Mode
(defun $dired-load-hook ()
  (setq dired-listing-switches "-lhA"
        dired-dwim-target t))

(req-package dired
  :ensure nil
  :config
  ($dired-load-hook)
  ;; Adding `$dired-load-hook' into `dired-load-hook' has no effect.  dired is
  ;; an essential package which may be loaded during the startup.
  )
