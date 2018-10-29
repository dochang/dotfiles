(req-package scroll-bar
  :ensure (scroll-bar :pin :built-in)
  :hook (emacs-startup . (lambda () (set-scroll-bar-mode 'left))))
