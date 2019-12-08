(req-package tooltip
  :ensure (tooltip :pin :built-in)
  :hook (emacs-startup . tooltip-mode))
