(setup (:package super-save)

  (add-hook 'emacs-startup-hook 'super-save-mode)

  (setq super-save-auto-save-when-idle t)

  (setq auto-save-default nil)

  )
