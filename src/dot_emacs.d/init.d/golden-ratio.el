(req-package golden-ratio
  :hook (emacs-startup . golden-ratio-mode)
  :init
  (setq golden-ratio-auto-scale t)
  (setq golden-ratio-exclude-modes '(calendar-mode)))
