(req-package color-theme
  :config
  (when after-init-time
    (color-theme-backup-original-values))
  ;; `color-theme' wants to run `color-theme-backup-original-values' in
  ;; `after-init-hook'.  But we can't, since the library would not be loaded at
  ;; that time.  We explicitly run this function after `color-theme' loaded.
  (color-theme-initialize)
  ;; NIL doesn't work any more.
  (setq color-theme-is-global t))
