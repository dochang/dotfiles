(req-package atomic-chrome
  :hook (emacs-startup . (lambda ()
                           (when (string= server-name "systemd")
                             (atomic-chrome-start-server))))
  :init
  (setq atomic-chrome-default-major-mode 'markdown-mode)
  (setq atomic-chrome-buffer-open-style 'split))
