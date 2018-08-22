(req-package df-mode
  :ensure nil
  :quelpa (df-mode :fetcher github :repo "emacsmirror/df-mode")
  :init
  (setq df-interval 10))
