(req-package tool-bar
  :ensure nil
  :hook (emacs-startup . (lambda () (tool-bar-mode -1))))
