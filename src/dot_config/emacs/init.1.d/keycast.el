(setup (:package keycast)

  (add-hook 'emacs-startup-hook 'keycast-tab-bar-mode)

  (:when-loaded

    (:option keycast-mode-line-remove-tail-elements nil)

    (:option keycast-header-line-remove-tail-elements nil)

    )

  )
