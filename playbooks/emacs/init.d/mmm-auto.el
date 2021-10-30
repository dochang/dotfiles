(req-package mmm-auto
  :ensure mmm-mode
  :hook (emacs-startup . (lambda () (require 'mmm-auto))))
