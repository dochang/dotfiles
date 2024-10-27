(setup mwheel

  (add-hook 'emacs-startup-hook 'mouse-wheel-mode)

  (:when-loaded

    (:option mouse-wheel-follow-mouse t)

    (:option mouse-wheel-progressive-speed t)

    )

  )
