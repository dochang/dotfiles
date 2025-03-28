(setup (:package super-save)

  (add-hook 'emacs-startup-hook 'super-save-mode)

  (:when-loaded

    (setopt super-save-auto-save-when-idle t)

    (setopt auto-save-default nil)

    )

  )
