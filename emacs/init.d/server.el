(req-package server
  :init
  (setq server-name (number-to-string (emacs-pid)))
  (setq server-use-tcp t))
