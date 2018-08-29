(req-package electric
  :hook (emacs-startup . (lambda () (electric-indent-mode -1)))
  ;; Disable by default since it doesn't work well in some modes such as
  ;; `yaml-mode'.
  )
