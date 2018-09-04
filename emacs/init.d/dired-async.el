(req-package dired-async
  :ensure nil
  :after dired
  :init
  (dired-async-mode))
