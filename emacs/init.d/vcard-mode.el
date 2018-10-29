(req-package vcard-mode
  :ensure (vcard-mode :pin :quelpa)
  :quelpa (vcard-mode :fetcher github :repo "dochang/vcard-mode")
  :mode ("\\.vc\\(f\\|ard\\)\\'" . vcard-mode))
