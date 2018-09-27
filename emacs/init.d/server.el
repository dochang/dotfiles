(req-package server
  :hook (emacs-startup . (lambda ()
                           (unless (bound-and-true-p server-process)
                             (server-start))))
  :custom
  (server-name (number-to-string (emacs-pid)))
  (server-use-tcp t))
