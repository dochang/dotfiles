(setup (:package guru-mode)

  (add-hook 'emacs-startup-hook #'guru-global-mode)

  (:when-loaded

    (setopt guru-warn-only nil)

    )

  )
