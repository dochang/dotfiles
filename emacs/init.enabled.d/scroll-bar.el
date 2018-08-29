(req-package scroll-bar
  :ensure nil
  :hook (emacs-startup . (lambda () (set-scroll-bar-mode 'left))))
