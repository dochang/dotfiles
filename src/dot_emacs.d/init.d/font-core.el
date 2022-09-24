(req-package font-core
  :ensure (font-core :pin :built-in)
  :hook (emacs-startup . global-font-lock-mode))
