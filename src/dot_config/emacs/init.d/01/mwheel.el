(setup mwheel

  (add-hook 'emacs-startup-hook #'mouse-wheel-mode)

  (:when-loaded

    (setopt mouse-wheel-follow-mouse t)

    (setopt mouse-wheel-progressive-speed t)

    )

  )
