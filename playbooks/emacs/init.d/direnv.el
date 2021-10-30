(req-package direnv
  :hook (emacs-startup . direnv-mode)
  :init
  (setq direnv-always-show-summary t)
  (setq direnv-show-paths-in-summary nil)
  (setq direnv-use-faces-in-summary t))
