(setup (:package which-key)

  (add-hook 'emacs-startup-hook 'which-key-mode)

  (:when-loaded

    (which-key-setup-side-window-right-bottom)

    (:option which-key-side-window-max-width 0.333)

    (:option which-key-sort-order 'which-key-key-order-alpha)

    )

  )
