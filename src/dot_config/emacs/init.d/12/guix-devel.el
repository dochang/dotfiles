(setup guix-devel

  (:only-if (locate-library "guix-devel"))

  (:with-mode (scheme-mode)
    (:hook guix-devel-mode))

  )
