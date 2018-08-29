(req-package smartparens
  :hook ((emacs-startup . smartparens-global-strict-mode)
         (emacs-startup . show-smartparens-global-mode))
  :config
  (require 'smartparens-config))
