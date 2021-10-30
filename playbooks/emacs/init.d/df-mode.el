(req-package df-mode
  :ensure (df-mode :pin :quelpa)
  :quelpa (df-mode :fetcher github :repo "emacsmirror/df-mode")
  :hook emacs-startup
  :init
  (setq df-interval 10))
