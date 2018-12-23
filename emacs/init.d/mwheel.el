(req-package mwheel
  :ensure (mwheel :pin :built-in)
  :hook (emacs-startup . mouse-wheel-mode)
  :custom
  (mouse-wheel-follow-mouse t)
  (mouse-wheel-progressive-speed t))
