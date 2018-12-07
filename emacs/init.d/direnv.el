(req-package direnv
  :hook (emacs-startup . direnv-mode)
  :custom
  (direnv-always-show-summary t)
  (direnv-show-paths-in-summary nil)
  (direnv-use-faces-in-summary t))
