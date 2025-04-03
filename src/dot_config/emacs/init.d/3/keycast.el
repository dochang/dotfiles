(setup (:package keycast)

  (add-hook 'emacs-startup-hook 'keycast-tab-bar-mode)

  (:when-loaded

    (setopt keycast-mode-line-remove-tail-elements nil)

    (setopt keycast-header-line-remove-tail-elements nil)

    )

  )
