(req-package vcard-mode
  :ensure nil
  :quelpa (vcard-mode :fetcher github :repo "dochang/vcard-mode")
  :mode ("\\.vc\\(f\\|ard\\)\\'" . vcard-mode))
