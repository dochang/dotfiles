(req-package super-save
  :hook (emacs-startup . super-save-mode)
  :custom
  (super-save-auto-save-when-idle t))
