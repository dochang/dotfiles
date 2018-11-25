(req-package golden-ratio
  :hook (emacs-startup . golden-ratio-mode)
  :custom
  (golden-ratio-auto-scale t)
  (golden-ratio-exclude-modes '(calendar-mode)))
