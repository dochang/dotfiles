(req-package server
  :hook (emacs-startup . (lambda ()
                           (unless (bound-and-true-p server-process)
                             (server-start))))
  :init
  (setq server-name (number-to-string (emacs-pid)))
  (setq server-use-tcp t))
