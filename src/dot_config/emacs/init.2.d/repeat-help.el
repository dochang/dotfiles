(setup (:package repeat-help)

  (:with-mode (repeat-mode)
    (:hook repeat-help-mode))

  (:when-loaded
    (setopt repeat-help-auto t))

  )
