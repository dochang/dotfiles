(req-package mmm-defaults
  :ensure mmm-mode
  :hook (emacs-startup . (lambda () (require 'mmm-defaults)))
  :config
  (setq mmm-global-mode 'maybe))
