(req-package df-mode
  :ensure nil
  :quelpa (df-mode :fetcher github :repo "emacsmirror/df-mode")
  :hook emacs-startup
  :custom
  (df-interval 10))
