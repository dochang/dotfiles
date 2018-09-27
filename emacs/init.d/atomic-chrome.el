(req-package atomic-chrome
  :hook (emacs-startup . (lambda ()
                           (when (string= server-name "systemd")
                             (atomic-chrome-start-server))))
  :custom
  (atomic-chrome-default-major-mode 'markdown-mode)
  (atomic-chrome-buffer-open-style 'split))
