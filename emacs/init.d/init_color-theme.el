(req-package color-theme
  :loader :elpa
  :config
  (color-theme-initialize)
  ;; NIL doesn't work any more.
  (setq color-theme-is-global t))
