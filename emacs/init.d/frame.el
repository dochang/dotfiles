(req-package frame
  :ensure nil
  :hook (emacs-startup . (lambda () (blink-cursor-mode -1))))
