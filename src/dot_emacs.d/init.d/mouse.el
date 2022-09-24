(req-package mouse
  :ensure (mouse :pin :built-in)
  :init
  (setq mouse-yank-at-point t))
