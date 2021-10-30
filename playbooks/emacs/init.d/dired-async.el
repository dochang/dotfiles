(req-package dired-async
  :ensure async
  :after dired
  :init
  (dired-async-mode))
