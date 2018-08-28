(req-package super-save
  :hook (emacs-startup . super-save-mode)
  :init
  (setq super-save-auto-save-when-idle t))
