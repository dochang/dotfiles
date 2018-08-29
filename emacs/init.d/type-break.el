(req-package type-break
  :hook (emacs-startup . (lambda () (type-break-mode -1)))
  ;; Use X Window to prevent RSI.
  )
