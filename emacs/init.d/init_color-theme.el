(req-package color-theme
  :config
  (color-theme-initialize)
  ;; NIL doesn't work any more.
  (setq color-theme-is-global t))
