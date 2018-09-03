(req-package dired-async
  :ensure nil
  :after dired
  :require async
  :init
  (dired-async-mode))
