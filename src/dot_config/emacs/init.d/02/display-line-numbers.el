(setup (:package display-line-numbers)

  (:with-mode (prog-mode conf-mode text-mode adoc-mode)
    (:hook display-line-numbers-mode))

  (:when-loaded
    (setopt display-line-numbers-type 'relative)
    (setopt display-line-numbers-grow-only nil)
    (setopt display-line-numbers-current-absolute t)
    (setopt display-line-numbers-widen nil))

  )
