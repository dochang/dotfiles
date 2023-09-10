(setup display-line-numbers

  (:with-mode (prog-mode conf-mode text-mode adoc-mode)
    (:hook display-line-numbers-mode))

  (:when-loaded
    (:option display-line-numbers-type t)
    (:option display-line-numbers-grow-only nil)
    (:option display-line-numbers-current-absolute nil)
    (:option display-line-numbers-widen nil))

  )
