(req-package mwheel
  :ensure (mwheel :pin :built-in)
  :hook (emacs-startup . mouse-wheel-mode)
  :init
  (setq mouse-wheel-follow-mouse t)
  (setq mouse-wheel-progressive-speed t))
