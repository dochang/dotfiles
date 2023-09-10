(setup (:package auto-package-update)

  (add-hook 'emacs-startup-hook 'auto-package-update-maybe)

  )
