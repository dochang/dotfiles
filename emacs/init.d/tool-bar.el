(req-package tool-bar
  :ensure (tool-bar :pin :built-in)
  :hook (emacs-startup . (lambda () (tool-bar-mode -1))))
