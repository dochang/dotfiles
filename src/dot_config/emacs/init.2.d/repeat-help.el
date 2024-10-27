(setup (:package repeat-help)

  (:with-mode (repeat-mode)
    (:hook repeat-help-mode))

  (:when-loaded
    (:option repeat-help-auto t))

  )
